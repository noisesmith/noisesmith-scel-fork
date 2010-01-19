// copyright 2003 stefan kersten <steve@k-hornz.de>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
// USA

EmacsDocument
// : Document
{
	classvar documentMap, <>current;
	var <>sceld;
	var title, path;
	var dataptr;
	var <isEdited, <isListener, <envir;

	*initClass {
		documentMap = IdentityDictionary.new;
		Class.initClassTree(EmacsInterface);
		Class.initClassTree(ScelDocument);
		EmacsInterface
		.put(\documentNew, { | id, makeEnvir |
			var doc;
			if (documentMap.includesKey(id).not) {
				//				[\documentNew, id, makeEnvir].postln;
				doc = this.prBasicNew.prInitFromLisp(id);
				// is this necessary?? when is this the case??
				if (makeEnvir.notNil) {
					doc.envir = Environment.new;
				};
			};
			nil
		})
		.put(\documentClosed, { | id |
			this.documentDo(id, { | doc |
// 				[\documentClosed, doc].postln;
				doc.sceld.closed;
			});
			nil
		})
		.put(\documentSetCurrent, { | id |
			if (current.notNil) {
// 				[\didResignKey, current].postln;
				current.didResignKey;
			};
			if (id.notNil) {
				this.documentDo(id, { | doc |
// 					[\didBecomeKey, doc].postln;
					doc.didBecomeKey;
				});
			};
			nil
		})
		.put(\documentSetProperty, { | id, msg, value |
			this.documentDo(id, { | doc |
// 				[\documentSetProperty, doc, msg, value].postln;
				doc.perform(msg, value);
			});
			nil
		});
	}

	getEmacsInfoAsync { | arglist, returnFunc |
		// allows us to get info from emacs asynchronously
		Emacs.sendToLisp(
			\_info,
			[ this ] ++ arglist,
			{ | resultList | returnFunc.value( *resultList ) }
		);
		^nil;
	}

	getEmacsInfoSync { | arglist |
		// only usable inside a routine, but matches the Document API
		var result = nil;
		var delta_t = 0.001;
		if(thisThread.class.asSymbol != \Routine,
			{ MethodError(
				"EmacsDocument synchronous methods can only be called" ++
				" inside a Routine" ).throw } );
		this.getEmacsInfoAsync( arglist, { | x | result = x } );
		while( { and( result.isNil, delta_t < 10 ) },
			{ sleep( delta_t = delta_t * 1.4 ) }
		);
		^result;
	}
	
	// synchronous, only in routines
	string { | rangestart, rangesize = 1 |
		^this.getEmacsInfoSync( [ \_string, rangestart, rangesize ] );
	}

	currentLine {
		^this.getEmacsInfoSync( [ \_currentLine ] );
	}

	currentBlock {
		^this.getEmacsInfoSync( [ \_currentBlock ] );
	}

	currentWord {
		^this.getEmacsInfoSync( [ \_currentWord ] );
	}

	selectedText {
		^this.getEmacsInfoSync( [ \_selectedText ] );
	}

	getBackgroundColor {
		^this.getEmacsInfoSync( [ \_background ] );
	}

	selectedRangeLocation {
		^this.getEmacsInfoSync( [ \_rangeLocation ] );
	}

	selectedRangeSize {
		^this.getEmacsInfoSync( [ \_rangeSize ] );
	}

	// async, usable outside routines
	stringAsync { | rangestart, returnFunc, rangesize = 1 |
		^this.getEmacsInfoAsync( [ \_string, rangestart, rangesize ],
			returnFunc );
	}

	currentLineAsync { | returnFunc |
		^this.getEmacsInfoAsync( [ \_currentLine ], returnFunc );
	}

	currentBlockAsync { | returnFunc |
		^this.getEmacsInfoAsync( [ \_currentBlock ], returnFunc );
	}

	currentWordAsync { |returnFunc|
		^this.getEmacsInfoAsync( [ \_currentWord ], returnFunc );
	}

	selectedTextAsync { | returnFunc |
		^this.getEmacInfoAsync( [ \_selectedText ], returnFunc );
	}

	getBackgroundColorAsync { | returnFunc |
		^this.getEmacsInfoAsync( [ \_background ], returnFunc );
	}

	selectedRangeLocationAsync { | returnFunc |
		^this.getEmacsInfoAsync( [ \_rangeLocation ], returnFunc );
	}

	selectedRangeSizeAsync { | returnFunc |
		^this.getEmacsInfoAsync( [ \_rangeSize ], returnFunc );
	}

	// from here down should be compatible with Document

	*documentDo { | id, function |
		var doc;
		doc = documentMap.at(id);
		^if (doc.notNil) {
			function.value(doc);
		}
	}
	*documentDoMsg { | id, selector ... args |
		var doc;
		doc = documentMap.at(id);
		^if (doc.notNil) {
			doc.performList(selector, args);
		}
	}

	// lisp support
	storeLispOn { | stream |
		dataptr.storeLispOn(stream)
	}

	// printing
	printOn { | stream |
		super.printOn(stream);
		stream << $( << this.title << $);
	}

	//document setup
	title_ { | argName, completionFunc |
		Emacs.sendToLisp(\_documentRename, [this, argName], {
			completionFunc.value(this);
		});
	}

	title{
		^title;
	}

	stringColor_ {arg color, rangeStart = -1, rangeSize = 0;
	}

	//interaction:
	front {
		Emacs.sendToLisp(\_documentSwitchTo, this);
	}

	unfocusedFront {
		Emacs.sendToLisp(\_documentPopTo, this);
	}

	syntaxColorize { | rangestart = -1, rangesize = 0 |
		Emacs.sendToLisp(\_documentSyntaxColorize,
			[ this, rangestart, rangesize ] );
	}

	selectRange { | start = 0, length = 0 |
		Emacs.sendToLisp(\_selectRange, [this, start, length]);
	}
	prisEditable_{ | flag = true |
		Emacs.sendToLisp(\_documentSetEditable, [this, flag]);
	}
	removeUndo{
		Emacs.sendToLisp(\_documentRemoveUndo, this);
	}
	string_{|string, rangestart = -1, rangesize = 1|
		Emacs.sendToLisp( \_documentPutString, [ this, string ] );
	}

	// environment support
	/*	envir_ { | environment |
		envir = environment;
		if (this === current) {
			envir.push;
		}
	}
	*/

	didBecomeKey {
		if (envir.notNil) {
			envir.push;
		};
		current = this;
	}

	didResignKey {
		if (envir === currentEnvironment) {
			envir.pop;
		};
		if ( current === this, { current = nil } );
		//super.didResignKey;
	}

	setBackgroundColor { | color |
		Emacs.sendToLisp( \_background_,
			[ this, color.red, color.green, color.blue ] );
	}

	setTextColor { | color, rangestart, rangesize |
		Emacs.sendToLisp( \_textColor_,
			[ this, color.red, color.green, color.blue,
				rangestart, rangesize ] );
	}


	insertTextRange { arg string, rangestart, rangesize;
		Emacs.sendToLisp( \_insertTextRange,
			[ this, string, rangestart, rangesize ] );
	}
	
	setFont {
		| rangestart, rangesize, family, height, weight, slant,
		overline, underline, strikethrough, box |
		Emacs.sendToLisp( \_font_,
			[ this, rangestart, rangesize, family, height, weight, slant,
				overline, underline, strikethrough, box ] );
	}


	// PRIVATE
	*prNewFromPath { | argPath, selectionStart, selectionLength, completionFunc |
		argPath = Document.standardizePath(argPath);
		Emacs.sendToLisp(
			\_documentOpen,
			[argPath, selectionStart + 1, selectionLength],
			{ | id |
				if (id.isNil) {
			 		"Couldn't create document".warn;
				}{
					this.documentDo(id, completionFunc);
				}
			});
	}
	*prNewFromString { | name, str, makeListener, completionFunc |
		Emacs.sendToLisp(
		 	\_documentNew,
			[name, str, makeListener],
			{ | id |
				if (id.isNil) {
					"Couldn't create document".warn;
				}{
					this.documentDo(id, completionFunc);
				}
			});
	}
	prInitFromLisp { | id |
		dataptr = id;
		this.prAdd;
	}
	prclose {
		if (dataptr.notNil) {
			Emacs.sendToLisp(\_documentClose, this);
		}
	}
	prAdd {
		ScelDocument.addToList( this );
		//		allDocuments = allDocuments.add(this);
		documentMap.put(dataptr, this);
		//initAction.value(this);
	}

	prRemove {
		ScelDocument.removeFromList( this );
		//	allDocuments.remove(this);
		documentMap.removeAt(dataptr);
		dataptr = nil;
	}

	prGetTitle {
		^title
	}
	prSetTitle { | argTitle |
		title = argTitle;
	}


	prGetFileName {
		^path
	}
	prSetFileName { | argPath |
		path = argPath;
		if (path.notNil) {
			path = Document.standardizePath(path);
		}
	}

	prSetIsListener { | flag |
		isListener = flag.notNil;
	}
	prSetEditable { | flag |
		//		sceld.editable = flag.notNil;
	}
	prSetEdited { | flag |
		isEdited = flag.notNil;
	}

	*prBasicNew { ^super.new }

	prselectLine { | line |
		Emacs.sendToLisp( \_selectLine, [ this, line ] );
	}

	prGetBounds { | returnFunc |
		^this.getEmacsInfo( [ \_bounds ], returnFunc );
	}

	prSetBounds { | rect |
		Emacs.sendToLisp( \_bounds_,
			[ this, rect.left, rect.top, rect.width, rect.height ] );
	}

	prinsertText { | dataptr, txt |
		Emacs.sendToLisp( \_insertText, [ this, dataptr, txt ] );
	}

	// unimplemented methods
/*
	// invalid methods
	initByIndex {
		^this.shouldNotImplement(thisMethod)
	}
	prinitByIndex {
		^this.shouldNotImplement(thisMethod)
	}
	initLast {
		^this.shouldNotImplement(thisMethod)
	}
	prGetLastIndex {
		^this.shouldNotImplement(thisMethod)
		}
*/
}

// EOF
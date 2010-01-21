// copyright 2007 Marije Baalman (nescivi AT gmail DOT com)
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

ScelDocument : Document{
	var <thisdoc;
	var cFuncs;
	var checkCurrent;
	var <envir;
	var title_p, path_p;

	*new{ | title = "Untitled", string = "", makeListener = false, toFront=true |
		//		"ScelDocument.new".postln;
		^super.prBasicNew.init( title, string, makeListener, toFront );
	}

	*open{  | path, selectionStart = 0, selectionLength = 0, toFront=true |
		^super.prBasicNew.initFromPath( path, selectionStart, selectionLength, toFront );
	}

	*newFromEmacs{ |doc|
		^this.prBasicNew.prinitFromEmacs( doc );
	}

	prinitFromEmacs{ |doc|
		thisdoc = doc;
		thisdoc.sceld = this;
		checkCurrent = { |doc| if ( EmacsDocument.current === doc, { this.didBecomeKey } ); };
		checkCurrent.value( doc );
		^this;
	}

	initFromPath{ | path, selectionStart = 0, selectionLength = 0, toFront=true|
		checkCurrent = { |doc| if ( EmacsDocument.current === doc, { this.didBecomeKey } ); };
		cFuncs = [checkCurrent];
		path_p = path;
		title_p = path;
		EmacsDocument.prNewFromPath(path, selectionStart, selectionLength, { |doc| thisdoc = doc; thisdoc.sceld = this; cFuncs.do{ |it| it.value(doc); } } );
		if ( toFront, { this.front } );
		^this
	}

	init{ |title, string, makeListener, toFront|
		//		"ScelDocument.init".postln;
		checkCurrent = { |doc|
			if ( EmacsDocument.current === doc,
				{ this.didBecomeKey } ) };
		cFuncs = [checkCurrent];
		title_p = title;
		EmacsDocument.prNewFromString(title, string, makeListener,
			{ |doc|
				thisdoc = doc;
				thisdoc.sceld = this;
				cFuncs.do{ |it| it.value(doc)}
			});
		if ( toFront, { this.front } );
		^this
	}

	string_ { | argName, rangestart = -1, rangesize = 1 |
		if ( thisdoc.notNil, {
			thisdoc.string_( argName, rangestart, rangesize )
		},{
			cFuncs = cFuncs ++ { this.string_( argName ) };
		});
	}

	title_ { | argName, completFunc |
		if ( thisdoc.notNil, {
			thisdoc.title_( argName, completFunc )
		},{
			cFuncs = cFuncs ++ { this.title_( argName, completFunc ) };
		});
	}

	getInfo { | arglist, callBack |
		thisdoc.getEmacsInfoAsync( arglist, callBack );
	}

	title{
		if ( thisdoc.notNil, {
			^thisdoc.title;
		},{
			^("***"++title_p++"***")
		});
	}

	// printing
	printOn { | stream |
		super.printOn(stream);
		stream << $( << this.title << $);
	}

	prGetFileName {
		if ( thisdoc.notNil, {
			^thisdoc.path;
		},{
			^path_p;
		});
	}
	prSetFileName { | argPath |
		"sceldoc.prSetFileName".postln;
		if ( thisdoc.notNil, {
			thisdoc.prSetFileName( argPath );
		},{
			cFuncs = cFuncs ++ { this.prSetFileName( argPath ) };
		});
	}

	prSetFileName_ { | argPath |
		"sceldoc.prSetFileName_".postln;
		path_p = argPath;
	}

	path_{ |path|
		"sceldoc.path".postln;
		this.prSetFileName( path );
		//		if ( thisdoc.notNil, { thisdoc.path_( path ) },{ completionFuncs = completionFuncs ++ { this.path_( path ) };  });
		//		^this
	}

	front {
		if ( thisdoc.notNil, {
			thisdoc.front
		},{
			cFuncs = cFuncs ++ { this.front };
		});
	}

	unfocusedFront {
		if ( thisdoc.notNil, {
			thisdoc.unfocusedFront;
		},{
			cFuncs = cFuncs ++ { this.unfocusedFront };
		});
	}
	syntaxColorize { | rangestart = -1, rangesize = 0 |
		if ( thisdoc.notNil, {
			thisdoc.syntaxColorize( rangestart, rangesize );
		},{
			cFuncs = cFuncs ++ { this.syntaxColorize };
		});
	}
	prisEditable_{ | flag = true |
		if ( thisdoc.notNil, {
			thisdoc.prisEditable_( flag );
		},{
			cFuncs = cFuncs ++ { this.prisEditable_( flag ) };
		});
		editable = flag;
	}

	removeUndo{
		if ( thisdoc.notNil, {
			thisdoc.removeUndo
		},{
			cFuncs = cFuncs ++ { this.removeUndo };
		});
	}

	envir_ { | environment |
		envir = environment;
		if (this === current) {
			envir.push;
		}
	}

	didBecomeKey {
		if (envir.notNil) {
			envir.push;
		};
		super.didBecomeKey;
		EmacsDocument.current = this;
	}

	didResignKey {
		if (envir === currentEnvironment) {
			envir.pop;
		};
		super.didResignKey;
	}

	//	envir_ { | environment | thisdoc.envir_( environment ) }
	//	didBecomeKey { thisdoc.didBecomeKey }
	//	didResignKey { thisdoc.didResignKey }

	closed {
		thisdoc.prRemove;
		onClose.value(this); // call user function
		//		allDocuments.remove(this);
		//		dataptr = nil;
	}

	isEdited {
		if ( thisdoc.notNil, {
			^thisdoc.isEdited
		},{
			^false;
		});
	}
	//	isFront { thisdoc.isFront }
	editable_{arg abool=true; this.prisEditable_( abool ) }

	/* should maybe be this:
	path{
		if ( thisdoc.notNil, {
			^^thisdoc.prGetFileName;
		},{
			^path_p;
		});
	*/

	path{^thisdoc.prGetFileName }

	*addToList{ |doc|
		var key, sceld;
		//		"adding to List".postln;
		key = allDocuments.detectIndex( { |it| it.thisdoc === doc } );
		if ( key.isNil,
			{
				sceld = ScelDocument.newFromEmacs( doc );
				allDocuments = allDocuments.add(sceld);
				initAction.value(sceld);
			});
	}
	*removeFromList{ |doc|
		var toremove;
		toremove = allDocuments.detectIndex( { |it| it.thisdoc === doc }  );
		if ( toremove.notNil,
			{
				allDocuments.removeAt(toremove);
			});
	}

	prclose {
		if ( thisdoc.notNil,{
			thisdoc.prclose
		},{
			cFuncs = cFuncs ++ { this.prclose };
		});
	}

	string {arg rangestart, rangesize = 1;
		^thisdoc.string( rangestart, rangesize );
	}

	currentLine {
		^thisdoc.currentLine();
	}

	currentWord {
		^thisdoc.currentWord();
	}

	currentBlock {
		^thisdoc.currentBlock();
	}


	text {
		^this.string;
	}

	rangeText { arg rangestart=0, rangesize=1;
		^this.string( rangestart, rangesize );
	}

	selectRange { | start=0, length=0 |
		thisdoc.selectRange( start, length );
		^nil;
	}

	selectedText {
		thisdoc.selectedText();
	}

	*postColor_{ | color |
		Emacs.sendToLisp(\_postColor, [ color.red, color.green, color.blue ] );
	}

	background {
		^thisdoc.getBackgroundColor();
	}

	background_ { | color |
		thisdoc.setBackgroundColor( color );
	}

	setTextColor { | color, rangeStart = -1, rangeSize = 0 | 
		thisdoc.setTextColor(color, rangeStart, rangeSize);
	}

	*prGetIndexOfListener{
		^this.allDocuments.detectIndex(
			{ |doc| doc.title == "*SCLang:PostBuffer*" } );
	}

	*current {
		var cur = EmacsDocument.current;
		if ( cur.isNil ){
			^cur;
		}{
			^cur.sceld;
		}
	}

	selectedRangeLocation {
		^thisdoc.selectedRangeLocation();
	}

	selectedRangeSize {
		^thisdoc.selectedRangeSize();
	}

	prselectLine { | line |
		thisdoc.prselectLine( line );
	}

	prGetBounds {
		^thisdoc.prGetBounds();
	}

	prSetBounds { | rect |
		thisdoc.prSetBounds( rect );
	}

	bounds {
		this.prGetBounds();
	}

	bounds_{ | rect |
		this.prSetBounds( rect );
	}

	prinsertText { | dataptr, txt |
		thisdoc.prinsertText( dataptr, txt );
	}

	insertTextRange { | string, rangestart, rangesize |
		thisdoc.insertTextRange( string, rangestart, rangesize );
	}

	setFont {
		| rangestart, rangesize, family, height, weight, slant,
		overline, underline, strikethrough, box |
		thisdoc.setFont( rangestart, rangesize, family, height, weight, slant,
			overline, underline, strikethrough, box );
	}

	font_ {
		| rangestart, rangesize, family, height, weight, slant,
		overline, underline, strikethrough, box |
		this.setFont( rangestart, rangesize, family, height, weight, slant,
			overline, underline, strikethrough, box );
	}

	// not implemented:

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
}

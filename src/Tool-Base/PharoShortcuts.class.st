"
I am a class defining the current shortcuts of the system. My main purpose is to avoid the duplication of shortcut definition amongst basic tools and to allow to change the shortcuts from a single place.

Users may query me to know the key combinations used to accept or cancel an action, browse or inspect an object.

EXAMPLES

PharoShortcuts current browseShortcut => Cmd-B

ATTENTION - I'm not still finished. My #current method returns always a new instance of myself instead of a configured one.

NEXT Steps: Allow one to configure the shortcuts and to define several shortcut configurations via settings.
"
Class {
	#name : #PharoShortcuts,
	#superclass : #Object,
	#category : #'Tool-Base'
}

{ #category : #accessing }
PharoShortcuts class >> current [

	^ self new
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> acceptShortcut [

	^ $s meta
]

{ #category : #'keymaps - code' }
PharoShortcuts >> argAdvanceShortcut [
	^ $a meta shift
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> backspaceShortcut [

	^ Character backspace asShortcut
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> backwardCharShortcut [

	^ Character arrowLeft asShortcut
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> backwardLineShortcut [

	^ Character arrowUp asShortcut
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> backwardWordDeleteShortcut [

	^ Character backspace shift
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> backwardWordShortcut [
	^ Character arrowLeft meta
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> browseClassReferencesShortcut [

	^ $n meta shift
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> browseImplementorsShortcut [

	^ $m meta
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> browseMethodHierarchyShortcut [

	^ $i meta
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> browseReferencesShortcut [

	^ $b meta shift
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> browseSendersShortcut [

	^ $n meta
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> browseShortcut [

	^ $b meta
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> browseVersionsShortcut [

	^ $v meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> cancelShortcut [

	^ $l meta
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> changeStyleShortcut [

	^ $k meta shift
]

{ #category : #'keymaps - window' }
PharoShortcuts >> closeWindowShortcut [

	^ $w meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> compareToClipboardShortcut [

	^ $c meta shift
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> copySelectionShortcut [

	^ $c meta
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> cursorEndShortcut [

	^ Character end meta
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> cursorHomeShortcut [

	^ Character home meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> cutShortcut [

	^ $x meta
]

{ #category : #'keymaps - code' }
PharoShortcuts >> debugItShortcut [

	^  $d meta shift
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> decreaseKerningShortcut [

	^ $9 meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> delShortcut [

	^ Character delete asShortcut
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> delWordShortcut [

	^ Character delete shift asShortcut
]

{ #category : #'keymaps - code' }
PharoShortcuts >> displayIfFalseShortcut [

	^ $f alt
]

{ #category : #'keymaps - code' }
PharoShortcuts >> displayIfTrueShortcut [

	^ $t alt win | $t alt unix | $t option mac
]

{ #category : #'keymaps - code' }
PharoShortcuts >> doItShortcut [

	^ $d meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> duplicateFromSelectionShortcut [

	^ $d meta shift
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> escapeShortcut [

	^ Character escape asShortcut
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> exchangeSelectionShortcut [

	^ $e meta
]

{ #category : #'keymaps - code' }
PharoShortcuts >> fileItInShortcut [

	^ $g alt win | $g alt unix | $g option mac
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> findAgainShortcut [
	^ $g meta
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> findShortcut [
	^ $f meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> formatCodeShortcut [

	^ $f meta shift
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> forwardCharShortcut [

	^ Character arrowRight asShortcut
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> forwardLineShortcut [

	^ Character arrowDown asShortcut
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> forwardWordShortcut [
	^ Character arrowRight meta
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> increaseKerningShortcut [

	^ $= meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> indentShortcut [

	^ $r meta shift
]

{ #category : #'keymaps - code' }
PharoShortcuts >> inspectItShortcut [

	^ $i meta
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> lineEndShortcut [

	^ Character end asShortcut
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> lineStartShortcut [

	^ Character home asShortcut
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> makeCapitalizedShortcut [

	^ $z meta shift
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> makeLowercaseShortcut [

	^ $x meta shift
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> makeUppercaseShortcut [

	^ $y meta shift
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> methodNamesContainingItShortcut [

	^  $w meta shift
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> methodStringContainingItShortcut [

	^  $e meta shift
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> normalEmphasisShortcut [

	^ $0 meta
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> openFontAlignmentMenuShortcut [

	^ $u meta
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> openFontColorMenuShortcut [

	^ $6 meta
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> openFontMenuShortcut [

	^ $k meta shift
]

{ #category : #'keymaps - Tools' }
PharoShortcuts >> openMonticielloShortcut [
	^ $o meta, $p meta
]

{ #category : #'keymaps - Tools' }
PharoShortcuts >> openPlaygroundShortcut [
	^ $o meta, $w meta
]

{ #category : #'keymaps - Tools' }
PharoShortcuts >> openSettingsShortcut [
	^ $o meta, $s meta
]

{ #category : #'keymaps - Tools' }
PharoShortcuts >> openSystemBrowserShortcut [
	^ $o meta , $b meta
]

{ #category : #'keymaps - Tools' }
PharoShortcuts >> openTestRunnerShortcut [
	^ $o meta , $u meta
]

{ #category : #'keymaps - Tools' }
PharoShortcuts >> openTranscriptShortcut [
	^ $o meta , $t meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> outdentShortcut [

	^ $l meta shift
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> pageDownShortcut [

	^ Character pageDown asShortcut
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> pageUpShortcut [

	^ Character pageUp asShortcut
]

{ #category : #'keymaps - code' }
PharoShortcuts >> pasteInitialsShortcut [

	^ $v alt win | $v alt unix | $v option mac
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> pasteShortcut [

	^ $v meta
]

{ #category : #'keymaps - code' }
PharoShortcuts >> printItShortcut [

	^ $p meta
]

{ #category : #'keymaps - code search' }
PharoShortcuts >> querySymbolShortcut [

	^  $q meta shift
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> redoShortcut [

	^ $z meta shift
]

{ #category : #'keymaps - code' }
PharoShortcuts >> removeShortcut [

	^ $x meta
]

{ #category : #'keymaps - debugger' }
PharoShortcuts >> restartExecutionShortcut [

	^ $a meta shift
]

{ #category : #'keymaps - debugger' }
PharoShortcuts >> resumeExecutionShortcut [

	^ $r meta
]

{ #category : #'keymaps - Tools' }
PharoShortcuts >> saveImageShortcut [
	^ $s meta shift
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> searchShortcut [
	^ $s meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> selectAllShortcut [

	^ $a meta
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectBackwardCharShortcut [

	^ Character arrowLeft shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectBackwardLineShortcut [

	^ Character arrowUp shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectBackwardWordShortcut [
	^ Character arrowLeft meta shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectForwardCharShortcut [

	^ Character arrowRight shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectForwardLineShortcut [

	^ Character arrowDown shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectForwardWordShortcut [
	^ Character arrowRight shift meta
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectLineEndShortcut [
	^ Character end shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectLineStartShortcut [
	^ Character home shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectPageDownShortcut [

	^ Character pageDown shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectPageUpShortcut [

	^ Character pageUp shift
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectTextEndShortcut [

	^ Character end shift meta
	 
]

{ #category : #'keymaps - editor selection' }
PharoShortcuts >> selectTextStartShortcut [
	^ Character home shift meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> selectUndoShortcut [

	^ $m meta shift
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> selectWordShortcut [

	^ Character space meta
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> setSearchStringShortcut [
	^ $h meta
]

{ #category : #'keymaps - debugger' }
PharoShortcuts >> stepIntoShortcut [

	^ $e meta 
]

{ #category : #'keymaps - debugger' }
PharoShortcuts >> stepOverShortcut [

	^ $o meta shift
]

{ #category : #'keymaps - debugger' }
PharoShortcuts >> stepThroughShortcut [

	^ $t meta shift
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> strokethroughShortcut [

	^ $= meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> swapCharsShortcut [

	^ $y meta
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> textEndShortcut [

	^ Character end meta
	 
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> textStartShortcut [

	^ Character home meta

]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> toggleBoldEmphasisShortcut [

	^ $7 meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> toggleCommentOnSelectionOrLine [

	^ $/ meta
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> toggleItalicEmphasisShortcut [

	^ $8 meta
]

{ #category : #'keymaps - editor style' }
PharoShortcuts >> underlineShortcut [

	^ $_ meta
]

{ #category : #'keymaps - editor edit' }
PharoShortcuts >> undoShortcut [

	^ $z meta
]

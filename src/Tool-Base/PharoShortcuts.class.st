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
	#category : #'Tool-Base-Keymapping'
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
	"Equivalent to: ctrl+backspace on Windows/Linux, option/alt+backspace on OSX"
	^ Character backspace meta
]

{ #category : #'keymaps - editor navigation' }
PharoShortcuts >> backwardWordShortcut [
	"Equivalent to: ctrl+left on Windows/Linux, option/alt+left on OSX"
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

	^ (OSPlatform current isMacOS)
		ifTrue: [ $d ctrl asShortcut ]
		ifFalse: [ Character delete ctrl asShortcut ]
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
	"Equivalent to: ctrl+right on Windows/Linux, option/alt+right on OSX"
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
	"Equivalent to: ctrl+shift+left on Windows/Linux, option/alt+shift+left on OSX"
	^ Character arrowLeft shift meta
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
	"Equivalent to: ctrl+shift+right on Windows/Linux, option/alt+shift+right on OSX"
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

{ #category : #'keymaps - window resizing' }
PharoShortcuts >> windowBottomEdgeDownShortcut [

	^ self windowManagementShortcut, $x shift meta, $x shift meta
]

{ #category : #'keymaps - window resizing' }
PharoShortcuts >> windowBottomEdgeUpShortcut [

	^ self windowManagementShortcut, $x shift meta, $s shift meta
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowBottomLeftShortcut [

	^ self windowManagementShortcut, $z
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowBottomShortcut [

	^ self windowManagementShortcut, $x
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowCenterShortcut [

	^ self windowManagementShortcut, $r
]

{ #category : #'keymaps - window resizing' }
PharoShortcuts >> windowLeftEdgeLeftShortcut [

	^ self windowManagementShortcut, $a shift meta, $a shift meta
]

{ #category : #'keymaps - window resizing' }
PharoShortcuts >> windowLeftEdgeRightShortcut [

	^ self windowManagementShortcut, $a shift meta, $s shift meta
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowLeftShortcut [

	^ self windowManagementShortcut, $a
]

{ #category : #'keymaps - window' }
PharoShortcuts >> windowManagementShortcut [

	^ $w shift meta
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowMaximizeShortcut [

	^ self windowManagementShortcut, $s
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowMinimizeShortcut [

	^ self windowManagementShortcut, $s shift
]

{ #category : #'keymaps - window moving' }
PharoShortcuts >> windowMoveDownShortcut [

	^ self windowManagementShortcut, Character arrowDown
]

{ #category : #'keymaps - window moving' }
PharoShortcuts >> windowMoveLeftShortcut [

	^ self windowManagementShortcut, Character arrowLeft
]

{ #category : #'keymaps - window moving' }
PharoShortcuts >> windowMoveRightShortcut [

	^ self windowManagementShortcut, Character arrowRight
]

{ #category : #'keymaps - window moving' }
PharoShortcuts >> windowMoveUpShortcut [

	^ self windowManagementShortcut, Character arrowUp
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowRightBottomShortcut [

	^ self windowManagementShortcut, $c
]

{ #category : #'keymaps - window resizing' }
PharoShortcuts >> windowRightEdgeLeftShortcut [

	^ self windowManagementShortcut, $d shift meta, $s shift meta
]

{ #category : #'keymaps - window resizing' }
PharoShortcuts >> windowRightEdgeRightShortcut [

	^ self windowManagementShortcut, $d shift meta, $d shift meta
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowRightShortcut [

	^ self windowManagementShortcut, $d
]

{ #category : #'keymaps - window resizing' }
PharoShortcuts >> windowTopEdgeDownShortcut [

	^ self windowManagementShortcut, $w shift meta, $s shift meta
]

{ #category : #'keymaps - window resizing' }
PharoShortcuts >> windowTopEdgeUpShortcut [

	^ self windowManagementShortcut, $w shift meta, $w shift meta
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowTopLeftShortcut [

	^ self windowManagementShortcut, $q
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowTopRightShortcut [

	^ self windowManagementShortcut, $e
]

{ #category : #'keymaps - window tiling' }
PharoShortcuts >> windowTopShortcut [

	^ self windowManagementShortcut, $w
]

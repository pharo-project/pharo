baseline
baseline: spec 
    
	<baseline>

	| repository |
	 
	repository := 'filetree://./pharo-core/src'.    
       
	spec for: #common do: [
		spec postLoadDoIt: #'postload:package:'.

		spec baseline: 'Morphic' with: [spec repository: repository].

		spec package: 'StartupPreferences'.
		
		spec package: 'DebuggerActions'.
		spec package: 'DebuggerFilters'.
		spec package: 'DebuggerModel'.

		spec package: 'FontChooser'.

		spec package: 'Tool-Base'.
		spec package: 'Transcript-Tool'.
		spec package: 'Tool-Workspace'.

		spec package: 'Spec-Core'.
		spec package: 'Spec-Debugger'.
		spec package: 'Spec-Inspector'.
		spec package: 'Spec-Layout'.
		spec package: 'Spec-MorphicAdapters'.
		spec package: 'Spec-PolyWidgets'.
		spec package: 'Spec-Tools'.
		spec package: 'Spec-StubAdapter'.
		
		spec package: 'Spec-Deprecated'.

		spec package: 'Morphic-Widgets-Tree'.

		spec package: 'Keymapping-Settings'.
		spec package: 'Keymapping-Tools-Spec'.

		spec package: 'WebBrowser-Core'.

		spec package: 'HelpSystem-Core'.

		spec package: 'Rubric'.
	].
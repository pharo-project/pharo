baseline
baseline: spec 
    
	<baseline>
        
	| repository | 
	
	repository := 'filetree://./pharo-core/src'.    

	spec for: #common do: [
		spec postLoadDoIt: #'postload:package:'.

		spec baseline: 'UI' with: [spec repository: repository].

		spec package: 'JenkinsTools-Core'.
		spec package: 'JenkinsTools-ExtraReports'.
		
		spec baseline: 'SUnit' with: [
			spec repository: repository.
			spec loads: #('UI' 'Help')].

		spec package: 'Morphic-Widgets-FastTable'.
		
		spec package: 'MessageBrowserRefactoringAddition'.
		spec package: 'Tool-ExternalBrowser'.
		spec package: 'HowToContributeHelp'.
		spec package: 'ContributingToTheCatalogHelp'.
		spec package: 'BlueInk-Core'. 
		spec package: 'BlueInk-Extras'.
		spec package: 'Tool-ProcessBrowser'.
		spec package: 'Tool-Profilers'.
		spec package: 'Tool-SystemReporter'.
		spec package: 'NECompletion'.
		spec package: 'Nautilus'.
		spec package: 'NautilusCommon'.
		spec package: 'Nautilus-GroupManager'.
		spec package: 'Nautilus-GroupManagerUI'.
		spec package: 'Komitter'.
		
		spec package: 'Announcements-Help'.	
		spec package: 'Metacello-FileTree'.
		spec package: 'Metacello-GitBasedRepository'.
		spec package: 'Metacello-GitHub'.
		spec package: 'Metacello-ProfStef'.
		spec package: 'Metacello-Reference'.
		spec package: 'Metacello-Tutorial'.
		spec package: 'MonticelloGUI'.
		spec package: 'Network-Mail'.
		spec package: 'Pharo-Help'.
		spec package: 'ProfStef-Core'.
		spec package: 'ProfStef-Help'.
		spec package: 'RecentSubmissions'.
		spec package: 'Refactoring-Changes'.
		spec package: 'Refactoring-Core'.
		spec package: 'Refactoring-Environment'.
		spec package: 'Regex-Help'.
		spec package: 'ImportingResource-Help'.
		spec package: 'IssueTracking'.
		spec package: 'Monticello-OldDataStreamCompatibility'. 
		spec package: 'MonticelloFileServices'.			
		spec package: 'Spec-Examples'.
		spec package: 'Spec-Help'.	
		spec package: 'Tool-Diff'.
		spec package: 'Tool-FileList'.
		spec package: 'Tool-Finder'.
		spec package: 'Tool-ImageCleaner'.		
		spec package: 'Tools'.
					
		spec package: 'Athens-Core'.
		spec package: 'Athens-CairoPools'.
		spec package: 'Athens-Cairo'.

	].
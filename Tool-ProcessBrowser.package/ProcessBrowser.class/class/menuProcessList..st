menu
menuProcessList: aBuilder 
	<contextMenu>
	<processBrowserProcessListMenu>

	| target selectedProcess |
	
	target := aBuilder model.
	selectedProcess := target selectedProcess.
	
	selectedProcess
		ifNotNil: [ 
			| nameAndRules |
			nameAndRules := target nameAndRulesForSelectedProcess.
			(aBuilder item: #'Inspect')
				keyText: 'i';
				selector: #inspectProcess.
			(aBuilder item: #'Explore')
				keyText: 'I';
				selector: #exploreProcess.
			(aBuilder item: #'Inspect Pointers')
				keyText: 'P';
				selector: #inspectPointers.
			(Smalltalk globals includesKey: #PointerExplorer)
				ifTrue: [ 
					(aBuilder item: #'Explore pointers')
						keyText: 'e';
						selector: #explorePointers ].
					
			nameAndRules second
				ifTrue: [ 
					(aBuilder item: #'Terminate')
						keyText: 't';
						selector: #terminateProcess.

					selectedProcess isSuspended
						ifTrue: [ 
							(aBuilder item: #'Resume')
								keyText: 'r';
								selector: #resumeProcess ]
						ifFalse: [ 
							(aBuilder item: #'Suspend')
								keyText: 's';
								selector: #suspendProcess ] ].
			
			nameAndRules third
				ifTrue: [ 
					(aBuilder item: #'Change priority')
						keyText: 'p';
						selector: #changePriority.
					(aBuilder item: #'Debug')
						keyText: 'd';
						selector: #debugProcess ].
			
			(aBuilder item: #'Profile messages')
				keyText: 'm';
				selector: #messageTally.

			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [ 
					(aBuilder item: #'Signal Semaphore')
						keyText: 'S';
						selector: #signalSemaphore ].
					
			(aBuilder item: #'Full stack')
				keyText: 'k';
				selector: #moreStack;
				withSeparatorAfter ].

	(aBuilder item: #'Find context...')
		keyText: 'f';
		selector: #findContext.
	(aBuilder item: #'Find again')
		keyText: 'g';
		selector: #nextContext;
		withSeparatorAfter.
		
	(aBuilder item: (target isAutoUpdating
			ifTrue: [ #'Turn off auto-update' ]
			ifFalse: [ #'Turn on auto-update' ]))
		keyText: 'a';
		selector: #toggleAutoUpdate.
	(aBuilder item: #'Update list')
		keyText: 'u';
		selector: #updateProcessList.

	Smalltalk globals 
		at: #CPUWatcher 
		ifPresent: [ :pw | 
			aBuilder withSeparatorAfter.
			pw isMonitoring
				ifTrue: [ 
					(aBuilder item: #'Stop CPUWatcher')
						selector: #stopCPUWatcher ]
				ifFalse: [ 
					(aBuilder item: #'Start CPUWatcher')
						selector: #startCPUWatcher ] ].
menu
changeSetMenu2: aBuilder
	<contextMenu>
	<changeSorterChangeSetList2Menu>
	
	| target cs |
	target := aBuilder model.
	cs := target selectedChangeSet.
	cs ifNil: [ ^self ].
	
	cs hasPreamble 
		ifTrue: [
			(aBuilder item: #'Edit preamble')
				keyText: 'p';
				selector: #addPreamble.
			(aBuilder item: #'Remove preamble')
				selector: #removePreamble ]
		ifFalse: [
			(aBuilder item: #'Add preamble')
				keyText: 'p';
				selector: #addPreamble ].

	cs hasPreamble 
		ifTrue: [
			(aBuilder item: #'Edit postscript...')
				selector: #editPostscript.
			(aBuilder item: #'Remove postscript')
				selector: #removePostscript ]
		ifFalse: [
			(aBuilder item: #'Add postscript...')
				selector: #editPostscript ].
			
		aBuilder withSeparatorAfter.
		(aBuilder item: #'Destroy change set')
			keyText: 'x';
			selector: #remove.
		
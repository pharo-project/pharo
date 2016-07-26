metacello
addMetacelloBaseline
	| baselineNumber existingBaselines msg selectorName addedPackages package stream packagePattern |
	"0 -- must have a repository selected"
	repository isNil
		ifTrue:
			[ UIManager default inform: 'Please select a repository and try again'.
			^ self ].	"1 -- Selecting baseline name"
	existingBaselines := self baseLines.
	msg := existingBaselines size > 0
		ifTrue: [ 'Current baselines:' , String cr
				,
					(existingBaselines
						inject: ''
						into: [ :sum :el | sum , el , String cr ])
				, 'Enter a new baseline number' ]
		ifFalse:
			[ 'Enter a new baseline number ("-baseline" will be automatically added)' ].
	baselineNumber := UIManager default request: msg initialAnswer: '1.0'.
	baselineNumber ifNil: [ ^ self ].	"2 -- add package names"
	addedPackages := OrderedCollection new.
	packagePattern := UIManager default
		request: 'Please enter a pattern to filter package names'
		initialAnswer: '*'.
	[ | packageNames workingCopies |
	packageNames := OrderedCollection new.
	workingCopies := OrderedCollection new.
	self workingCopies
		do: [ :pkg | 
			((packagePattern match: pkg package name)
				and: [ (addedPackages includes: pkg package name) not ])
				ifTrue: [ packageNames add: pkg package name.
					workingCopies add: pkg ] ].
	package := UIManager default
		chooseFrom: packageNames
		values: workingCopies
		title: 'Add dependent package (cancel to stop)'.
	package ifNotNil: [ addedPackages add: package package name ] ]
		doWhileTrue: [ package notNil ].	"3 -- Creating baseline"
	selectorName := baselineNumber copyWithoutAll: '.-'.
	stream := WriteStream on: String new.
	stream nextPutAll: 'baseline'.
	stream nextPutAll: selectorName.
	stream
		nextPutAll:
			': spec 
        <version: '''.
	stream nextPutAll: baselineNumber.
	stream
		nextPutAll:
			'-baseline''>
        
        spec for: #common do: [
                spec blessing: #baseline.
                spec repository: '''.
	stream nextPutAll: repository description.
	stream
		nextPutAll:
			'''.    
                "spec 
                        package: ''Example-Core'';
                        package: ''Example-Tests'' with: [ spec requires: ''Example-Core'' ]"
'.
	addedPackages
		do: [ :pName | 
			stream
				nextPutAll: '			spec package: ''';
				nextPutAll: pName;
				nextPutAll: '''.';
				nextPutAll: String cr ].
	stream nextPutAll: '                ].'.
	self configurationClass
		compile: stream contents
		classified: 'baselines'
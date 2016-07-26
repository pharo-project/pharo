metacello
createVersionForBlessing: blessingSelector
	"blessingSelector = #release or #development"

	| existingVersions msg initialAnswer newVersionNumber selectorName stream mcWorkingCopy |
	existingVersions := self configurationClass methods
		select: [ :cm | cm selector beginsWith: 'version' ].
	existingVersions := (existingVersions
		collect: [ :m | m pragmas first argumentAt: 1 ]) asSortedCollection.
	msg := ''.
	existingVersions size > 4
		ifTrue: [ msg := 'Last 4 versions: '
				,
					(existingVersions
						copyFrom: existingVersions size - 4
						to: existingVersions size) asArray printString , String cr ].
	initialAnswer := existingVersions size > 0
		ifTrue: [ existingVersions last , '.1' ]
		ifFalse: [ '1.0' ].
	newVersionNumber := UIManager default
		request: msg
		initialAnswer: initialAnswer.
	newVersionNumber ifNil: [ ^ self ].
	selectorName := newVersionNumber copyWithoutAll: '.-'.
	stream := WriteStream on: String new.
	stream nextPutAll: 'version'.
	stream nextPutAll: selectorName.
	stream
		nextPutAll:
			': spec 
        <version: '''.
	stream nextPutAll: newVersionNumber.
	stream
		nextPutAll: ''' imports: #(''';
		nextPutAll: self lastBaseLine;
		nextPutAll:
			''')>

        spec for: #common do: [
                spec blessing: ';
		nextPutAll: blessingSelector printString;
		nextPutAll:
			'.
'.
	self dependentPackages
		do: [ :pName | 
			stream
				nextPutAll: '			spec package: ''';
				nextPutAll: pName;
				nextPutAll: ''' with: '''.
			mcWorkingCopy := (MCPackage named: pName) workingCopy.
			mcWorkingCopy needsSaving
				ifTrue: [ self
						inform:
							'The configuration you want to save depends on the package ' , pName
								, '. You first need to save this package in order to create the version'.
					^ self ].
			stream nextPutAll: mcWorkingCopy ancestry ancestors first name.
			stream
				nextPutAll: '''.';
				nextPutAll: String cr ].
	stream nextPutAll: '                ].'.
	self configurationClass
		compile: stream contents
		classified: 'versions'
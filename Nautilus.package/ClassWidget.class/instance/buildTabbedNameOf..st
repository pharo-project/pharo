private
buildTabbedNameOf: anElement
			
	| tab size stringMorph |
	
	tab := '   '.
	size := self cachedHierarchy indentationFor: anElement.
	
	stringMorph := (size isZero 
		ifTrue: [ anElement name ]
		ifFalse: [ ( tab repeat: size ) , anElement name ]) asStringMorph.
		
	self model selectedPackage ifNotNil: [ :package |
		(package includesClass: anElement) ifFalse: [ 
			stringMorph color: self model extensionColor ] 
	].
	
	anElement hasAbstractMethods ifTrue: [ 	
		stringMorph 
		emphasis: 2; 
		color: (self abstractColorAdjust: stringMorph color)
	].
	
	^ stringMorph
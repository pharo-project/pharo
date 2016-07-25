private
wrapFlat: anElement
	| result |
	result := anElement name.
	^ self model selectedPackage
		ifNil: [ result ]
		ifNotNil: [ :package | 
			(package includesClass: anElement)
				ifTrue: [ result asStringMorph ]
				ifFalse: [ 
					result asStringMorph
						color: self model extensionColor;
						yourself ] ]
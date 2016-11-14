private
methodWrapper: anItem
	| package selector result emphasis |
	selector := anItem selector.
	package := self model selectedPackage.
	result := selector asMorph.
	emphasis := 0.
	anItem protocol asString first = $*
		ifTrue: [ 
			| item |
			item := anItem protocol allButFirst asLowercase.
			(item = package name asLowercase
				or: [ (item beginsWith: package name asLowercase) and: [ (item at: package name size + 1) = $- ] ])
				ifFalse: [ 
					result := selector asMorph
						color: self model extensionColor;
						yourself ] ]
		ifFalse: [ 
			((package extendedClasses includes: anItem methodClass) or: [ (package includesClass: anItem methodClass) not ])
				ifTrue: [ 
					result := selector asMorph
						color: self model extensionColor;
						yourself ] ].
	result := self handleGroupWrappingFor: selector and: result in: package.
	anItem isFromTrait
		ifTrue: [ 
			| trait |
			trait := anItem origin.
			selector := selector , ' (' , trait name , ')'.
			result := selector asMorph.
			emphasis := emphasis + 2 ].
	self model showInstance
		ifFalse: [ emphasis := emphasis + 1 ].
	anItem isDeprecated ifTrue: [ emphasis := emphasis + 2r10000 ].	
	^ result emphasis: emphasis
private
categoryWrapper: anItem
	| package class result |

	package := self model selectedPackage.
	class := self model selectedClass.
	result := anItem asMorph.
	anItem first == $*
		ifTrue: [ | item |
			item := anItem allButFirst asLowercase.
			((item = package name asLowercase) or: 
				[ (item beginsWith: package name asLowercase) and: [ (item at: (package name size +1)) =$-]])
				ifFalse: [ result := anItem asMorph color: self model extensionColor; yourself ]]
		ifFalse: [
			((package extendedClasses includes: class) or: [ (package includesClass: class) not ])
				ifTrue: [ result := anItem asMorph color: self model extensionColor; yourself ]].
	self model showInstance
		ifFalse: [ result emphasis: 1 ].
	^ result
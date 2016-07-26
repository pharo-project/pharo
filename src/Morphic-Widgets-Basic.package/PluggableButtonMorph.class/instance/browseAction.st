private
browseAction

	| classDefiningAction |
	classDefiningAction := self model class whichClassIncludesSelector: self actionSelector.
	Smalltalk tools browser openOnClass: classDefiningAction selector: self actionSelector.
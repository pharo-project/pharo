instance creation
class: aClass 
	| newInstance |
	newInstance := self basicNew initialize.
	newInstance setClass: aClass.
	^ newInstance
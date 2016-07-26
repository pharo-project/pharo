initialize-release
class: aClass selector: aSelector interval: anInterval 
	interval := anInterval.
	class := self classObjectFor: aClass.
	selector := aSelector
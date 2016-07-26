private
class: aClass selector: aSelector to: examples
	(aClass includesSelector: aSelector) ifFalse: [ 
		^ (GTExampleDeclaredMethodMissing new
			methodClass: aClass;
			selector: aSelector;
			yourself) signal ].
	self class: aClass selector: aSelector method: aClass >> aSelector to: examples
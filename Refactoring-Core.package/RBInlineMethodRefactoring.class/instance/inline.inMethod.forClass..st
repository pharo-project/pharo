initialize-release
inline: anInterval inMethod: aSelector forClass: aClass 
	sourceSelector := aSelector.
	class := self classObjectFor: aClass.
	sourceInterval := anInterval
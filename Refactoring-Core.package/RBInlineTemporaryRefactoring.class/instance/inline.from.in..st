initialize-release
inline: anInterval from: aSelector in: aClass 
	class := self classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval
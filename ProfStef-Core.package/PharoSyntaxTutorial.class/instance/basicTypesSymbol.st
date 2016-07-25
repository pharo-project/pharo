lessons
basicTypesSymbol
	^ Lesson
title: 'Basic types: Symbols' 
lesson: 
'"A Symbol is a String which is guaranteed to be globally unique. 

There is one and only one Symbol #ProfStef. There may be several ''ProfStef'' String objects.

(Message == returns true if the two objects are the SAME)"

''ProfStef'' asSymbol.

#ProfStef asString.

(2 asString) == (2 asString).

(2 asString) asSymbol == (2 asString) asSymbol.


(Smalltalk globals at: #ProfStef) next.'
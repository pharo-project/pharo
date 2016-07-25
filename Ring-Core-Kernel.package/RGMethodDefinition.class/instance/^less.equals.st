comparing
<= aRGMethodDefinition
	"Sort method definitions according to: 1. class name - 2. class comment - 3. method selector - 4. timestamp"

	self parentName < aRGMethodDefinition parentName ifTrue: [^true].
	self parentName > aRGMethodDefinition parentName ifTrue: [^false].

	self flag: 'This ugly test is needed right now because the old system is used to represent 
	class comment as method with Comment selector. And to mix comment and methods'.
	aRGMethodDefinition isComment ifTrue: [^false].
	
	^(self selector < aRGMethodDefinition selector) 
 		  or: [ (self selector == aRGMethodDefinition selector) and: [
    			  self timeStamp <= aRGMethodDefinition timeStamp ]]
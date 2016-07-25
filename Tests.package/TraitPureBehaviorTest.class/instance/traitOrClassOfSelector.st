testing
traitOrClassOfSelector
	"self run: #traitOrClassOfSelector"

	"locally defined in trait or class"

	self assert: (self t1 traitOrClassOfSelector: #m12) = self t1.
	self assert: (self c1 traitOrClassOfSelector: #foo) = self c1.

	"not locally defined - simple"
	self assert: (self t4 traitOrClassOfSelector: #m21) = self t2.
	self assert: (self c2 traitOrClassOfSelector: #m51) = self t5.

	"not locally defined - into nested traits"
	self assert: (self c2 traitOrClassOfSelector: #m22) = self t2.

	"not locally defined - aliases"
	self assert: (self t6 traitOrClassOfSelector: #m22Alias) = self t2.

	"class side"
	self assert: (self t2 classSide traitOrClassOfSelector: #m2ClassSide:) 
				= self t2 classSide.
	self assert: (self t6 classSide traitOrClassOfSelector: #m2ClassSide:) 
				= self t2 classSide
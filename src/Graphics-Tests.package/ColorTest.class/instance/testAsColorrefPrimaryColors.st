tests
testAsColorrefPrimaryColors
"issue 13784 Color>>#asColorref ignores blue"
	self assertColorrefFor: Color black equals: 16r000000;
	assertColorrefFor: Color red equals: 16r0000FF;
	assertColorrefFor: Color green equals: 16r00FF00;
	assertColorrefFor: Color yellow equals: 16r00FFFF;
	assertColorrefFor: Color blue equals: 16rFF0000;
	assertColorrefFor: Color cyan equals: 16rFFFF00;
	assertColorrefFor: Color magenta  equals: 16rFF00FF;
	assertColorrefFor: Color white equals: 16rFFFFFF
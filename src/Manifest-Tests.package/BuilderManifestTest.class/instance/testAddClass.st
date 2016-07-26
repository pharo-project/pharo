tests
testAddClass
	
	| manifestBuilder|
	
	manifestBuilder := TheManifestBuilder of: MFClassA .
	
	self deny: ((manifestBuilder rejectClasses) anySatisfy: [:each| each = MFClassA]).
	
	manifestBuilder addRejectClass: MFClassA..
	
	self assert: ((manifestBuilder rejectClasses) anySatisfy: [:each| each = MFClassA]).

	manifestBuilder removeRejectClass: MFClassA.	.

	self deny: ((manifestBuilder rejectClasses) anySatisfy: [:each| each = MFClassA]).
	
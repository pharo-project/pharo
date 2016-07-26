menu
specifyDashedLine

	| executableSpec newSpec |
	executableSpec := UIManager default
		request:
'Enter a dash specification as
{ major dash length. minor dash length. minor dash color }
The major dash will have the normal border color.
A blank response will remove the dash specification.
[Note: You may give 5 items as, eg, {10. 5. Color white. 0. 3}
where the 4th ityem is zero, and the 5th is the number of pixels
by which the dashes will move in each step of animation]' translated
		initialAnswer: '{ 10. 5. Color red }'.
	executableSpec isEmptyOrNil ifTrue:
		[^ self stopStepping; dashedBorder: nil].
	newSpec := [self class compiler evaluate: executableSpec] ifError:
		[^ self stopStepping; dashedBorder: nil].
	(newSpec first isNumber and: [newSpec second isNumber and: [newSpec third isColor]]) ifFalse:
		[^ self stopStepping; dashedBorder: nil].
	newSpec size = 3 ifTrue:
		[^ self stopStepping; dashedBorder: newSpec].
	(newSpec size = 5 and: [newSpec fourth isNumber and: [newSpec fifth isNumber]]) ifTrue:
		[^ self dashedBorder: newSpec; startStepping].

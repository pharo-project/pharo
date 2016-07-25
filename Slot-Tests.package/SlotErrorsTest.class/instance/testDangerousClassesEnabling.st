tests
testDangerousClassesEnabling

	| savedExistingSystemSetting myExpectedError |

	savedExistingSystemSetting := DangerousClassNotifier enabled.

	DangerousClassNotifier disable.
	self assert: DangerousClassNotifier enabled not.
	DangerousClassNotifier disableDuring: [
      self assert: DangerousClassNotifier enabled not].
	self assert: DangerousClassNotifier enabled not.

	DangerousClassNotifier enable.
	self assert: DangerousClassNotifier enabled.	
	DangerousClassNotifier disableDuring: [
      self assert: DangerousClassNotifier enabled not].
	self assert: DangerousClassNotifier enabled.

	DangerousClassNotifier enable.
	DangerousClassNotifier disableDuring: [
		self deny: (DangerousClassNotifier check: #CompiledMethod)].
	myExpectedError := false.
	[ DangerousClassNotifier check: #CompiledMethod ] on: Warning do: [ myExpectedError := true  ].
	self assert: myExpectedError.

	savedExistingSystemSetting 
		ifTrue: [ DangerousClassNotifier enable ]
		ifFalse: [ DangerousClassNotifier disable ].
	self assert: DangerousClassNotifier enabled equals: savedExistingSystemSetting.

stack list
messageTally
	| secString secs |
	secString := UIManager default request: 'Profile for how many seconds?' initialAnswer: '4'.
	secString ifNil: [secString := String new].
	secs := secString asNumber asInteger.
	(secs isNil
			or: [secs isZero])
		ifTrue: [^ self].
	[ Smalltalk tools timeProfiler spyOnProcess: selectedProcess forMilliseconds: secs * 1000 ] forkAt: selectedProcess priority + 1.
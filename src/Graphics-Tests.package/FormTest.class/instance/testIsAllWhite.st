tests
testIsAllWhite	"self run: #testIsAllWhite"
	"Make sure #isAllWhite works for all bit depths"
	
	#(-32 -16 -8 -4 -2 -1 1 2 4 8 16 32) do:[:d| | form |
		form := Form extent: 16@16 depth: d.
		form fillBlack.
		self deny: form isAllWhite.
		form fillWhite.
		self assert: form isAllWhite.
	].

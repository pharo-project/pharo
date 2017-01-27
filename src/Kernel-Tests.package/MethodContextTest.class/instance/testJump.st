tests
testJump
	#(exampleClosure exampleSend exampleStore) do: [ :selector |
		self verifyJumpWithSelector: selector ]
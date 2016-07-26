instance creation
signature: signature "<String>" block: aBlock "<BlockClosure> ^<Callback>"
	"Answer a Callback that evaluates aBlock whose C signature is signature 
	 and whose thunk should conform to the C calling convention."
	^self newForPlatform signature: signature block: aBlock
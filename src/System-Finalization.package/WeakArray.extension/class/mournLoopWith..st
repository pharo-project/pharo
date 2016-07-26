*System-Finalization
mournLoopWith: firstMourner
	"Send mourn to all the objects available in the mourn queue, starting
	 with firstMourner which the sender has already extraced for us.  If
	 an error occurs here, it will break this loop but the sender will spawn
	 another mournLoopWith: so that finalization is not broken by errors in
	 individual cases."
	| mourner |
	mourner := firstMourner.
	[mourner mourn.
	 (mourner := self primitiveFetchMourner) notNil] whileTrue
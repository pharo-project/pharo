example
example
	"POP3Client example"
	"download a user's messages into an OrderedCollection and inspect the OrderedCollection"

	| ps messages userName password |
	userName := (UIManager default request: 'POP username').
	password := (UIManager default request: 'POP password').
	ps := POP3Client openOnHostNamed: (UIManager default request: 'POP server').
	[
	ps loginUser: userName password: password.
	ps logProgressToTranscript.

	messages := OrderedCollection new.
	1 to: ps messageCount do: [ :messageNr |
		messages add: (ps retrieveMessage: messageNr) ]]
		ensure: [ps close].

	messages inspect.
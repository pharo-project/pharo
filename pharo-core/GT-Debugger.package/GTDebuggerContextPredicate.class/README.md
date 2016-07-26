I encapsulate a condition that can be verified against a Context.

To add concrete conditions a subclass should be create that overrides the method #matchContext:

Public API and Key Messages
- #matches: 
- #hasMatched
- #result

Instance Variables
result:		the last value returned by matches:


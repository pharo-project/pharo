I am ZnServerGenericLogEvent, a ZnServerLogEvent and ZnLogEvent that holds a generic subject, possibly not limited to a String.

Here are two examples:

(ZnServerGenericLogEvent subject: 'You can''t do that, Dave') emit.

(ZnServerGenericLogEvent subject: { #id->123. #reason->#timeout } asDictionary) emit.
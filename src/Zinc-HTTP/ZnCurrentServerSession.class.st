"
I am ZnCurrentServerSession.
I am a ProcessLocalVariable and a ProcessSpecificVariable.

I hold a reference to the current ZnServerSession instance during HTTP processing, during the dynamic scope of #handleRequest:

ZnServerSessions are created lazily.

"
Class {
	#name : #ZnCurrentServerSession,
	#superclass : #ProcessLocalVariable,
	#category : #'Zinc-HTTP-Variables'
}

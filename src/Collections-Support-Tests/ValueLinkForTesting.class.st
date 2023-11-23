"
I am a variant of `ValueLink` to show the behavior of `Object>>#~~>`.

In particular, I will be used to test expressions of the form `a ~~> b` 
that create a link (with `a` as value) of the same class of the link `b`, 
so I don't introduce any new methods but provide myself to check that newly 
created links keep the class of the chain we are pushing on.
"
Class {
	#name : 'ValueLinkForTesting',
	#superclass : 'ValueLink',
	#category : 'Collections-Support-Tests-Links',
	#package : 'Collections-Support-Tests',
	#tag : 'Links'
}

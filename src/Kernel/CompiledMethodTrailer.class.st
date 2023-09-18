"
CompiledMethodTrailer was removed in Pharo12

It was responsible for encoding and decoding various kinds of compiled method trailer data, but that had many downsides:
- variable length trailers forced often called methods like CompiledMethod>>#endPC to instantiate an instance
- setting a sourcePointer (or embedding sources) needed to create a new compiled method and do a become
- to avoid that, the compiler supported to use an existing trailer and delegated instance creation of CompiledMethod to it.
- using 1 byte for the trailer kind lead to the need use Variable length encoding already for our ~40MB source file for some methods.

All in all, the CompiledMethodTrailer added a lot of complexity for nothing.

What do we do now?
- use use a fixed-length trailer (see #trailerSize)
- writing and reading that is not that complex (see #setSourcePointer:, #sourcePointer)

Q: I used to hand a trailer to the compiler, what should I do now?

The compiler now creates always CompiledMethods with a fixed, but empty trailer. Just use #sourcePointer: to set it after.

Q: I used to clear the trailer using #copyWithTrailerBytes:, what should I do now?

There is now #clearSourcePointer. Just use #copy before if you want a copy.

Q: I want to have a non-installed method, but with source code.
The compiler now sets a #source property (and setting a source pointer via #sourcePointer: removes it).
If you use the compiler, you need to do nothing special anymore! All methods come with source via the property by default.

"
Class {
	#name : 'CompiledMethodTrailer',
	#superclass : 'Object',
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'kinds' }
CompiledMethodTrailer class >> empty [
	^self deprecated: 'CompiledMethodTrailer has been removed in Pharo12. Just use sourcePointer / sourcePointer to read and set. See class comment of CompiledMethodTrailer for more information'
]

{ #category : 'testing' }
CompiledMethodTrailer class >> isDeprecated [
	^ true
]

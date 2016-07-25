Any morphs embedded in the text should use me as a wrapper.

This class is private (you are not supposed to create 
its instances by yourself).

This morph wraps a user morph, embedded in a text,
and provides some helpers for the morph's positioning etc.

An example of using an embedded object can be found in #exampleWithEmbeddedObject

To insert sanembedded object send #insertObject: 
to a text position, for example:
text endPosition insertObject: 123.
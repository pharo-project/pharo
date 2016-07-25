Use cascaded nextPutAll:''s instead of #, in #nextPutAll:.
Indeed this is really important to understand that the implementation of the concatenation of strings creates a new string for each uses of ,.
Therefore if you use five , to concatenate an expression you create 5 intermediate strings for nothing. Using nextPutAll: just add the string in the stream.

Have a look at  streamContents:. Here is an example

String streamContents: [ :s|
		s nextPutAll: '---'.
		s nextPutAll: self.
		s nextPutAll: '---' ].
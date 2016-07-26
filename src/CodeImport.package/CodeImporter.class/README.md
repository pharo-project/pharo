I'm an object in charge of import source files.  I know a format object that knows how to parse the files, and I import the parsed results into the image.

I handle doIts normally, but some special cases like class organization, class comment or methods are handled via a double dispatch (See my method extensions for that).

=-=-=- How to use me -=-=-=

If you want to fileIn the code -aka compile/evaluate it:

CodeImporter evaluateReadStream: '2+2!' readStream.

or

CodeImporter evaluateString: '2+2!'

or

CodeImporter evaluateFileNamed: 'something.st'

or

CodeImporter evaluateFileStream: (FileStream readOnlyFileNamed: 'something.st')

Now, you can also generate a model of code declarations inside a file by just creating an instance:

CodeImporter fileStream: (FileStream readOnlyFileNamed: 'something.st').

And then query me sending the message

#codeDeclarations

Instances of me can be created through 
#fromString:
#readStream:
#fileNamed:
#fileStream:
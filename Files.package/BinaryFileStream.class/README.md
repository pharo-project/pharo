I am a concrete subclass of AbstractBinaryFileStream for normal files. Regardless the position of the file, I will make my operarions on my position and then return the file it's own position.

In addition to my superclass' API I provide the following methods.

stream upToEnd
"reads the full stream up to the end and returns the contents"
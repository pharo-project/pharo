A simulation of a FileStream, but living totally in memory.  Hold the contents of a file or web page from the network.  Can then fileIn like a normal FileStream.

Need to be able to switch between binary and text, as a FileStream does, without recopying the whole collection.  Convert to binary upon input and output.  Always keep as text internally.
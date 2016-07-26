ZipReadStream is intended for uncompressing the compressed contents of Zip archive members.

Since Zip archive members keep their expected CRC value separately in Zip headers, this class does not attempt to read the CRC from its input stream.

Instead, if you want the CRC verification to work you have to call #expectedCrc: with the expected CRC-32 value from the Zip member header.
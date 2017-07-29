I am a write stream wrapping a second stream. Whenever they ask me to write a cr, a lf, or a crlf I'll instead print a portable new line depending on the platform I'm on.

stream := '' writeStream.
converter := ZnCrPortableWriteStream on: stream.
converter cr; cr; lf; nextPut: $a.
stream contents
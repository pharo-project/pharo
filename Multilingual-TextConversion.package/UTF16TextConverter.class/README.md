Text converter for UTF-16.  It supports the endianness and byte order mark.

The default is to not use a BOM, make sure to set this if using the converter to write to a location where encoding metadata is not present / specified by the protocol.

This usually means you want to write one when storing strings to files on disk, but not when transmitting encoded string to a web client, nor when sending data to a database.
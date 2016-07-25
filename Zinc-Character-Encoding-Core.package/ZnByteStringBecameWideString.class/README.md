I am ZnByteStringBecameWideString, a resumable Error signalled to indicate that some byteString was changed to a wideString.

Used by ZnUTF8Encoder>>#readInto:startingAt:count:fromStream: to avoid a #becomeForward: when a ByteString automagically changes into a WideString.

Part of Zinc HTTP Components.
I am ZnNullEncoder, a concrete subclass of ZnCharacterEncoder.
I perform no encoding or decoding at all for all characters with a code value below 256.

Note that in principle I could handle Latin1 (ISO-8859-1) or ASCII, although that is not completely correct. To get maximum efficiency, it remains an option.
	
Part of Zinc HTTP Components.
fields description
fieldsDesc
	^#(
		" The type of binary data for this file. "
		LGitDiffBinaryTypeEnum type;

		" The binary data, deflated. "
		LGitExternalUInt8Array data;

		" The length of the binary data. "
		#FFIUInt32 datalen;

		" The length of the binary data after inflation. "
		#FFIUInt32 inflatedlen;
	)
initialize-release
textResponseWide64k
	message := ZnResponse ok: (ZnEntity with: (self randomUnicodeWideString: 64 * 1024))
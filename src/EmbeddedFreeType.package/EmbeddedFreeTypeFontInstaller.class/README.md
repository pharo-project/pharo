An EmbeddedFreeTypeFontInstaller installs embedded free type fonts in a provider.
It registers itself as a font installer to FreeTypeFontProvider. And when someone asks to update fonts from system, EmbeddedFreeTypeFontInstaller installs corresponding fonts.

Instance Variables
	embeddedFileInfoCache:		<Dictionary>
	provider:		<FreeTypeFontProvider>

embeddedFileInfoCache
	- internal cache

provider
	- a provider which asks for installing the embedded fonts.
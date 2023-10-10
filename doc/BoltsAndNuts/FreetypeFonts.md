# Freetype Fonts

This help explains how to interact with FreetypeFonts. It explains how to embed a font in the system, how to load it to use it. And how to update the fonts to see all the fonts installed in the system.

## Introduction

FreeType is a font processing library present in many systems ([http://www.freetype.org](http://www.freetype.org)). This package is the FFI binding to use the library. The library is present in the build of the Pharo VM for all the suported platforms. 

It allows us to load and use a variety of font formats, also it is integrated with the host OS to use the installed files in the system. 

It supports fonts of the following formats:

- TrueType fonts (TTF) and TrueType collections (TTC)
- CFF fonts
- WOFF fonts
- OpenType fonts (OTF, both TrueType and CFF variants) and OpenType collections (OTC)
- Type 1 fonts (PFA and PFB)
- CID-keyed Type 1 fonts
- SFNT-based bitmap fonts, including color Emoji
- X11 PCF fonts
- Windows FNT fonts
- BDF fonts (including anti-aliased ones)
- PFR fonts
- Type 42 fonts (limited support)

## Loading Fonts

Freetype allows us to load a lot of different font formats. The format of the file is handled by the library and also it is automatically detected. 

To create a Font that can be used in Pharo, it is enough to execute the following script:

```st
aFont := FreeTypeFont fromFile: './Purisa-Bold.ttf' pointSize: 15.
```

A font in Pharo has always a pointsSize, also as some file formats can have more than a font in a file, there is a option to load a file with an index.

```st
aFont := FreeTypeFont fromFile: './Purisa-Bold.ttf' pointSize: 15 index: 0.
```

To see the content, you should only inspect the file.

## Loading Fonts from ByteArray

We can also load a font from a ByteArray, this allows us to load a font from the internet (using Zinc) or other sources. 

For embedding fonts in an image there is a special engine. You should use it. Please refer to the section in this help.

### For creating a font you should use:

```st
FreeTypeFont 
	fromBytes: aByteArray 
	pointSize: aSize.
```

or if there are different fonts in the ByteArray you should use the index option.

```st
FreeTypeFont 
	fromBytes: aByteArray 
	pointSize: aSize 
	index: 0.
```

We can have an example loading a font from the internet:

```st
FreeTypeFont 
	fromBytes: (ZnEasy get:'https://github.com/6/font-cdn/blob/master/public/fonts/Montserrat-700.woff?raw=true') contents 
	pointSize: 20 
```

## Embedding Fonts

When developing an application in Pharo, sometimes is useful to include custom fonts.
This custom fonts need to be loaded during the startup of the image
 
The best way to include the fonts is to embed them in the image, as there is already all the problem solve. The image includes two ways of loading fonts during the startup of the image:

- Loading from the file system (from the System font directory)
- Loading from a ByteArray containing the whole font.

To load a font during startup, you should create a subclass of `EmbeddedFreeTypeFontDescription`.
These subclasses should implement three messages in the class side.

- `canBeInstalled` returns true if the font can be installed (if the contents is a byteArray, it can always be installed, but if it is get from the filesystem, this message should check if the file exists and can be read.)

- `fontContents` it returns a byteArray with the contents of the font. It can be from a byteArray constant or from other place. 

- `originalFileName` it should return the original name of the file embedded.

For an example of implementing the loading of fonts, you can check the existing subclasses of `EmbeddedFreeTypeFontDescription` that implements this behavior.

Once the fonts are automatically loaded, they can be referenced by its name.

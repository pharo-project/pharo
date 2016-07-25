I am a facade and a global reference to actual fonts to be used system wide in a number of predefined categories.

I also provide methods to interactively choose these fonts, to reset all fonts to some default value, to set larger demo fonts, to construct a font menu and to manipulate the current settings as an external specification.

The font categories that I know of are: ButtonFont, CodeFont, HaloFont, ListFont, MenuFont and WindowTitleFont

Examples:
- I open a dialog to choose the font for buttons with:
StandardFonts chooseButtonFont.

- To change base size for all fonts (when you want big fonts for a screencast or a presentation):
StandardFonts setDemoFonts.

- Then restore default fonts with:
StandardFonts restoreDefaultFonts.
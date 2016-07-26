This class provides the support for the languages in 'Latin-2' category.  Although we could have different language environments for different languages in the category, so far nobody seriously needed it.

  I (Yoshiki) don't have good knowledge in these language, so when Pavel Krivanek volunteered to implement the detail, it was a good test to see how flexible my m17n framework was.  There are a few glitches, but with several email conversations over a few days, we managed to make it work relatively painlessly.  I thought this went well.

  There seem that some source of headache, as Windows doesn't exactly use Latin-2 encoded characters, but a little modified version called 'code page 1250'.  Similar to Japanese support, the encode interpreters are swapped based on the type of platform it is running on.


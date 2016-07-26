A MCLazyVersionInfo puts ancestor and stepChilds data in WeakArrays to allow their memory to be reclaimed and reloaded on demand.

For all purposes, a MCLazyVersionInfo behaves exactly like a MCVersionInfo with the same data, but:
- let the ancestry (and step children) be garbage collected
- and reload that ancestry (and stepChildren) from the MC repositories when accessed (MCLazyVersionInfo>>loadAncestorsAndStepChildren for the reload, MCLazyVersionInfo>>ancestors and MCLazyVersionInfo>>stepChildren for the access).

MCLazyVersionInfo is installed just as the ancestor in a MCWorkingAncestry (the direct ancestry of a WorkingCopy, which is the state all packages take when they are loaded in an image). It allows a MCWorkingAncestry to still know which package it is coming from, and, of course, the MCLazyVersionInfo to be able to reload the relevant ancestry from the right package.

Once installed, by flushing the Monticello package cache, all the stored ancestry is garbage collected, except the roots of those ancestry trees, which are the MCLazyVersionInfo instances inside the MCWorkingAncestry instances.

Warnings:

- Algorithms which traverse the entire ancestry chain of all packages will force a reload of all the packages contained in the image, which is a time and memory and network consuming process...

- It degrades gracefully when reloading becomes impossible (returns an empty ancestry, keeps knowledge of the fact some ancestry is missing, reloading a full ancestry on when possible). This has been tested.



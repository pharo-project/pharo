copying
postCopy
    super postCopy.
    blessing := blessing copy.
    description := description copy.
    author := author copy.
    timestamp := timestamp copy.
    packageList := packageList copy
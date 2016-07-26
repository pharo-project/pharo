development support
goferBranch: branchName project: commitMessage
    | pkgSpec |
    pkgSpec := self projectPackage.
    pkgSpec file: pkgSpec name , '.' , branchName.
    ^ pkgSpec goferBranchPackage: branchName message: commitMessage
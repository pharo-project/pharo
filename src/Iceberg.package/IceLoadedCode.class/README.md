The loaded code of a repository can have three different shapes:
- Nothing is loaded yet (represented by an instance of me).
- All loaded packages using versions from the same commit (should be the most common situation).
- You cherry-picked versions comming from differents commits (not recomended but supported).

As commitish, I point to the code as it was loaded from the repository. Notice that after that moment both the code in the image as well as the origin of the code could have changed (in case that I load a branch, the branch can have new commits after I loaded it).
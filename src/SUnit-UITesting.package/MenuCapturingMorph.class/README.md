It's difficult to test whether menus appear because the test might block the UI thread and prevent it.

I use Morphic stepping to monitor the world for a new menu. If I find one, you can get it by sending me #menu.
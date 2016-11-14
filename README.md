# automirror

Sometimes in life you need to access external hosted repositories from behend a corporate firewall. In a lot of cases the programming language's dependency management and build tooling is designed to believe silly things like proxies don't exist. In almost all cases they don't work with authentication out of the box, and if they do at all its only with Basic auth. So there are a couple of options, run a personal authenticating proxy (eg, cntlm). An alternative would be to mirror the repository that you need, unfortunately the tooling most language repos provide for mirroring is bespoke and complicated. New language, new repo mirror tool. Who has the patience. 

Automirror is a low config reverse proxy based mirror targetted at easy to create and manage mirroring of package/library repositories.



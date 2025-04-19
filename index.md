---
title: "A History of Bandra—A Collective Archival Project"
---

Lorem ipsum dolor sit amet from Bandora to Bandra consectetur adipiscing elit.
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.


## Browse Collections

### By Medium:

::: by-medium
* [Maps](#)
* [Illustrations](#)
* [Photographs](#)
:::

:::: columns

::: {.column .by-period}

### By Period:

$for(periods)$
* [$periodStart$-$periodEnd$: $title$]($url$)
$-endfor$

:::

::: {.column .by-theme}

### By Theme:

$for(themes)$
* [$title$]($url$)
$-endfor$

:::

::::


:::: columns

::: {.column .nearly-there}

### Nearly There

$for(nearly-there)$
* [$title$]($url$)
$-endfor$

:::

::: {.column .drafts}

### Drafts

$for(drafts)$
* [$title$]($url$)
$-endfor$

:::


::: {.column .untagged}

### Untagged

$for(untagged)$
* [$title$]($url$)
$-endfor$

:::

::::

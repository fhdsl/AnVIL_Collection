# AnVIL Module Library

There are several pieces of content that are useful across multiple books, such as "How to Create a Workspace".  To reduce maintenance burden, the `AnVIL_Template` repository has a collection of `_child` R Markdown documents that can be included in your books.

You can see all the available AnVIL modules in the [AnVIL_Template book](https://jhudatascience.org/AnVIL_Template/)

## Include using `cow::borrow_chapter()`

To add a module to your book, include a code chunk with the `cow::borrow_chapter()` function, specifying the file for the module you want to borrow and the repository you want to borrow it from (the `AnVIL_Template` repository).

See the OTTR_Template [wiki page](https://github.com/jhudsl/OTTR_Template/wiki/Borrowing-chapters-between-courses) for the most recent instructions and additional details.

Here is an example of including the `_child_workspace_create.Rmd` with `cow::borrow_chapter()`


1. [Launch Terra](https://anvil.terra.bio/#workspaces)

1. In the drop-down menu on the left, navigate to "Workspaces". Click the triple bar in the top left corner to access the menu. Click "Workspaces".

    <img src="03-AnVIL_modules_files/figure-html//1zyqZHITAthNhXeH2XQqA7FMOu2mek6wfgGEaje1KQsk_g117989bd49c_0_150.png" title="Screenshot of Terra drop-down menu.  The &quot;hamburger&quot; button to extend the drop-down menu is highlighted, and the menu item &quot;Workspaces&quot; is highlighted." alt="Screenshot of Terra drop-down menu.  The &quot;hamburger&quot; button to extend the drop-down menu is highlighted, and the menu item &quot;Workspaces&quot; is highlighted." width="480" />

1. Click on the **plus icon** near the top of left of the page.

    <img src="03-AnVIL_modules_files/figure-html//1zyqZHITAthNhXeH2XQqA7FMOu2mek6wfgGEaje1KQsk_g117989bd49c_0_733.png" title="Screenshot of Terra Workspaces page.  The &quot;+&quot; is highlighted." alt="Screenshot of Terra Workspaces page.  The &quot;+&quot; is highlighted." width="480" />

1. Name your Workspace and select the appropriate Billing Project.  **All activity in the Workspace will be charged to this Billing Project** (regardless of who conducted it).

    <img src="03-AnVIL_modules_files/figure-html//1zyqZHITAthNhXeH2XQqA7FMOu2mek6wfgGEaje1KQsk_g117989bd49c_0_877.png" title="Screenshot of Terra dialog box for creating a new Workspace.  The text box labeled &quot;Workspace name&quot; and the drop-down menu labeled &quot;Billing project&quot; are highlighted." alt="Screenshot of Terra dialog box for creating a new Workspace.  The text box labeled &quot;Workspace name&quot; and the drop-down menu labeled &quot;Billing project&quot; are highlighted." width="480" />

1. If you are working with protected data, you can set the **Authorization Domain** to limit who can be added to your Workspace.  Note that the Authorization Domain cannot be changed after the Workspace is created (i.e. there is no way to make this Workspace shareable with a larger audience in the future).  Workspaces by default are only visible to people you specifically share them with.  Authorization domains add an extra layer of enforcement over privacy, but by nature make sharing more complicated.  We recommend using Authorization Domains in cases where it is extremely important and/or legally required that the data be kept private (e.g. protected patient data, industry data).  For data you would merely prefer not be shared with the world, we recommend relying on standard Workspace sharing permissions rather than Authorization Domains, as Authorization Domains can make future collaborations, publications, or other sharing complicated.

    <img src="03-AnVIL_modules_files/figure-html//1zyqZHITAthNhXeH2XQqA7FMOu2mek6wfgGEaje1KQsk_g117989bd49c_0_1022.png" title="Screenshot of Terra dialog box for creating a new Workspace.  The drop-down menu labeled &quot;Authorization domain&quot; is highlighted." alt="Screenshot of Terra dialog box for creating a new Workspace.  The drop-down menu labeled &quot;Authorization domain&quot; is highlighted." width="480" />

1. Click "CREATE WORKSPACE".  The new Workspace should now show up under your Workspaces.

    <img src="03-AnVIL_modules_files/figure-html//1zyqZHITAthNhXeH2XQqA7FMOu2mek6wfgGEaje1KQsk_g117989bd49c_0_1166.png" title="Screenshot of Terra dialog box for creating a new Workspace.  The &quot;Create Workspace&quot; button is highlighted." alt="Screenshot of Terra dialog box for creating a new Workspace.  The &quot;Create Workspace&quot; button is highlighted." width="480" />

## Background Highlighting for Borrowed Chapters

You can use the `borrowed_chunk` [custom div block](https://bookdown.org/yihui/rmarkdown-cookbook/custom-blocks.html) to highlight borrowed content or any other content you choose.

**Basic format:**

```
:::: {.borrowed_chunk}
Code/Content here
::::
```

**Rendered with highlighting:**

:::: {.borrowed_chunk}

If you do not already have a Google account that you would like to use for accessing Terra, [create one now](https://accounts.google.com/SignUp).

If you would like to create a Google account that is associated with your non-Gmail, institutional email address, follow [these instructions](https://support.terra.bio/hc/en-us/articles/360029186611).
::::

**Rendered without highlighting:**


If you do not already have a Google account that you would like to use for accessing Terra, [create one now](https://accounts.google.com/SignUp).

If you would like to create a Google account that is associated with your non-Gmail, institutional email address, follow [these instructions](https://support.terra.bio/hc/en-us/articles/360029186611).

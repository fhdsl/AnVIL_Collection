# Billing

Modules about billing and Billing Projects on Google Cloud Platform and Terra.

<br>

## Create Google Billing Account

:::: {.borrowed_chunk}

1. Log in to the [Google Cloud Platform](https://console.cloud.google.com/) console using your Google ID. **Make sure to use the same Google account ID you use to log into Terra.**  

1. If you are a first time user, don’t forget to claim your free credits! If you haven't been to the console before, once you accept the Terms of Service you will be greeted with an invitation to "Try for Free."

    <img src="05-billing_modules_files/figure-html//1tGpzZaQxoTAcxs_nyyNL2FOqypjEofEnMVpBtZiAw4A_g116ca06e27d_0_0.png" title="Screenshot of the Google Cloud Console with the &quot;Try for Free&quot; button highlighted." alt="Screenshot of the Google Cloud Console with the &quot;Try for Free&quot; button highlighted." width="480" />

1. Follow the instructions to sign up for a Billing Account and get your credits. 

1. Choose “Individual Account”. This “billing account” is just for managing billing, so you don’t need to be able to add your lab members. You will need to give either a credit card or bank account for security. Don't worry! **You won't be billed until you explicitly turn on automatic billing**.

    <img src="05-billing_modules_files/figure-html//1tGpzZaQxoTAcxs_nyyNL2FOqypjEofEnMVpBtZiAw4A_g116ca06e27d_0_146.png" title="Screenshot of the Google Cloud Billing Account Setup, with &quot;Individual Account&quot; highlighted.  Also highlighted is text stating &quot;You won't be charged unless you manually upgrade to a paid account.&quot;" alt="Screenshot of the Google Cloud Billing Account Setup, with &quot;Individual Account&quot; highlighted.  Also highlighted is text stating &quot;You won't be charged unless you manually upgrade to a paid account.&quot;" width="480" />

1. You can view and edit your new Billing Account, by selecting “Billing” from the left-hand menu, or going direction to the billing console [console.cloud.google.com/billing](https://console.cloud.google.com/billing) 

    <img src="05-billing_modules_files/figure-html//1tGpzZaQxoTAcxs_nyyNL2FOqypjEofEnMVpBtZiAw4A_g116ca06e27d_0_293.png" title="Screenshot of the Google Cloud Console dropdown menu, with &quot;Billing&quot; highlighted." alt="Screenshot of the Google Cloud Console dropdown menu, with &quot;Billing&quot; highlighted." width="480" />

1. Clicking on the Billing Account name will allow you to manage the account, including accessing reports, setting alerts, and managing payments and billing.  We will cover account management in greater detail below.

    <img src="05-billing_modules_files/figure-html//1tGpzZaQxoTAcxs_nyyNL2FOqypjEofEnMVpBtZiAw4A_g116ca06e27d_0_437.png" title="Screenshot of the Google Cloud Console Billing Page, with the name of the new billing account highlighted." alt="Screenshot of the Google Cloud Console Billing Page, with the name of the new billing account highlighted." width="480" />

At any point, you can create additional Billing Accounts using the **Create Account** button.  We recommend creating a new Billing Account for each funding source.
::::

## Add Terra to Google Billing Account

:::: {.borrowed_chunk}

This gives Terra permission to create projects and send charges to the Google Billing Account, and must be done by an administrator of the Google Billing Account.

Terra needs to be added as a "Billing Account User":

1. Log in to the [Google Cloud Platform](https://console.cloud.google.com/) console using your Google ID.
1. Navigate to [Billing](https://console.cloud.google.com/billing)

    <img src="05-billing_modules_files/figure-html//1OqSVH5Y4v97-OKMnEDGGuhKBJwc9fyXc-Q1-ivbeZmA_g115e284bdc2_0_144.png" title="Screenshot of the Google Cloud Console drop-down menu, with &quot;Billing&quot; highlighted." alt="Screenshot of the Google Cloud Console drop-down menu, with &quot;Billing&quot; highlighted." width="480" />

1. You may be automatically directed to view a specific Billing Account.  If you see information about a single account rather than a list of your Billing Accounts, you can get back to the list by clicking "Manage Billing Accounts" from the drop-down menu.
 
    <img src="05-billing_modules_files/figure-html//1OqSVH5Y4v97-OKMnEDGGuhKBJwc9fyXc-Q1-ivbeZmA_g115e284bdc2_0_295.png" title="Screenshot of an individual Google Cloud Billing Account with the drop-down menu item &quot;Manage Billing Accounts&quot; highlighted." alt="Screenshot of an individual Google Cloud Billing Account with the drop-down menu item &quot;Manage Billing Accounts&quot; highlighted." width="480" />

1. Check the box next to the Billing Account you wish to add Terra to, click "ADD MEMBER".

    <img src="05-billing_modules_files/figure-html//1OqSVH5Y4v97-OKMnEDGGuhKBJwc9fyXc-Q1-ivbeZmA_g116e2c647a7_0_151.png" title="Screenshot of Google Cloud Billing Accounts Overview. The checkbox next to the name of a Billing Account is checked and highlighted, and the &quot;Add Member&quot; button is highlighted." alt="Screenshot of Google Cloud Billing Accounts Overview. The checkbox next to the name of a Billing Account is checked and highlighted, and the &quot;Add Member&quot; button is highlighted." width="480" />

1. Enter `terra-billing@terra.bio` in the text box.  In the drop-down menu, mouse over Billing, then choose "**Billing Account User**".

    <img src="05-billing_modules_files/figure-html//1OqSVH5Y4v97-OKMnEDGGuhKBJwc9fyXc-Q1-ivbeZmA_g116e2d0fe67_0_0.png" title="Screenshot of the dialogue box for adding a member to a Google Cloud Billing Accounts. The text box is highlighted and has been filled in with &quot;terra-billing@terra.bio&quot;.  In the drop-down menu labeled &quot;Select a Role&quot;, the item &quot;Billing&quot; and the submenu item &quot;Billing Account User&quot; are highlighted." alt="Screenshot of the dialogue box for adding a member to a Google Cloud Billing Accounts. The text box is highlighted and has been filled in with &quot;terra-billing@terra.bio&quot;.  In the drop-down menu labeled &quot;Select a Role&quot;, the item &quot;Billing&quot; and the submenu item &quot;Billing Account User&quot; are highlighted." width="480" />

1. Click "SAVE".

    <img src="05-billing_modules_files/figure-html//1OqSVH5Y4v97-OKMnEDGGuhKBJwc9fyXc-Q1-ivbeZmA_g116e2d0fe67_0_146.png" title="Screenshot of the dialogue box for adding a member to a Google Cloud Billing Accounts. The Save button is highlighted." alt="Screenshot of the dialogue box for adding a member to a Google Cloud Billing Accounts. The Save button is highlighted." width="480" />
::::

## Add Members to Google Billing Account

:::: {.borrowed_chunk}

Anyone you wish to add to the Billing Account will need their own Google ID.

To add a member to a Billing Project:
  
1. Log in to the [Google Cloud Platform](https://console.cloud.google.com/) console using your Google ID.
1. Navigate to [Billing](https://console.cloud.google.com/billing)

    <img src="05-billing_modules_files/figure-html//1j1wRbaDyHJJhZQcWcP4xeYgIoaIBRIF3LIZpiDPBumw_g115e284bdc2_0_144.png" title="Screenshot of the Google Cloud Console drop-down menu, with &quot;Billing&quot; highlighted." alt="Screenshot of the Google Cloud Console drop-down menu, with &quot;Billing&quot; highlighted." width="480" />

1. You may be automatically directed to view a specific Billing Account.  If you see information about a single account rather than a list of your Billing Accounts, you can get back to the list by clicking "Manage Billing Accounts" from the drop-down menu.

    <img src="05-billing_modules_files/figure-html//1j1wRbaDyHJJhZQcWcP4xeYgIoaIBRIF3LIZpiDPBumw_g115e284bdc2_0_295.png" title="Screenshot of an individual Google Cloud Billing Account with the drop-down menu item &quot;Manage Billing Accounts&quot; highlighted." alt="Screenshot of an individual Google Cloud Billing Account with the drop-down menu item &quot;Manage Billing Accounts&quot; highlighted." width="480" />

1. Check the box next to the Billing Account you wish to add a member to, click "ADD MEMBER".

    <img src="05-billing_modules_files/figure-html//1j1wRbaDyHJJhZQcWcP4xeYgIoaIBRIF3LIZpiDPBumw_g116e2c647a7_0_151.png" title="Screenshot of Google Cloud Billing Accounts Overview. The checkbox next to the name of a Billing Account is checked and highlighted, and the &quot;Add Member&quot; button is highlighted." alt="Screenshot of Google Cloud Billing Accounts Overview. The checkbox next to the name of a Billing Account is checked and highlighted, and the &quot;Add Member&quot; button is highlighted." width="480" />

1. Enter their Google ID in the text box. In the drop-down menu, mouse over Billing, then choose the appropriate role.

    <img src="05-billing_modules_files/figure-html//1j1wRbaDyHJJhZQcWcP4xeYgIoaIBRIF3LIZpiDPBumw_g116e2c647a7_0_296.png" title="Screenshot of the dialogue box for adding a member to a Google Cloud Billing Accounts. In the drop-down menu labeled &quot;Select a Role&quot;, the item &quot;Billing&quot; and the submenu item &quot;Billing Account Viewer&quot; are highlighted." alt="Screenshot of the dialogue box for adding a member to a Google Cloud Billing Accounts. In the drop-down menu labeled &quot;Select a Role&quot;, the item &quot;Billing&quot; and the submenu item &quot;Billing Account Viewer&quot; are highlighted." width="480" />

1. Click "SAVE".

    <img src="05-billing_modules_files/figure-html//1j1wRbaDyHJJhZQcWcP4xeYgIoaIBRIF3LIZpiDPBumw_g116e2c647a7_0_441.png" title="Screenshot of the dialogue box for adding a member to a Google Cloud Billing Accounts. The Save button is highlighted." alt="Screenshot of the dialogue box for adding a member to a Google Cloud Billing Accounts. The Save button is highlighted." width="480" />
::::

## Set Alerts for Google Billing

:::: {.borrowed_chunk}

1. Log in to the [Google Cloud Platform](https://console.cloud.google.com/) console using the Google ID associated with your Google Cloud projects.

1. Open the dropdown menu on the top left and click on [Billing](https://console.cloud.google.com/billing).

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_144.png" title="Screenshot of the Google Cloud Console drop-down menu, with &quot;Billing&quot; highlighted." alt="Screenshot of the Google Cloud Console drop-down menu, with &quot;Billing&quot; highlighted." width="480" />

1. You may be automatically directed to view a specific Billing Account. If you see information about a single account (and it’s not the one you’re interested in), you can get back to the list of all your Billing Accounts by clicking "Manage Billing Accounts" from the drop-down menu.

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_295.png" title="Screenshot of an individual Google Cloud Billing Account with the drop-down menu item &quot;Manage Billing Accounts&quot; highlighted." alt="Screenshot of an individual Google Cloud Billing Account with the drop-down menu item &quot;Manage Billing Accounts&quot; highlighted." width="480" />

1. Click on the name of the Billing Account you want to set alerts for.

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_150.png" title="Screenshot of Google Cloud Billing Accounts Overview. A Billing Account name is highlighted." alt="Screenshot of Google Cloud Billing Accounts Overview. A Billing Account name is highlighted." width="480" />

1. In the left-hand menu, click "Budgets & alerts".
 
    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_442.png" title="Screenshot of an individual Google Cloud Billing Account with the left-hand menu item &quot;Budgets &amp; alerts&quot; highlighted." alt="Screenshot of an individual Google Cloud Billing Account with the left-hand menu item &quot;Budgets &amp; alerts&quot; highlighted." width="480" />

1. Click the "Create Budget" tab.

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_587.png" title="Screenshot of the budgets and alerts page for a Google Cloud Billing Account. The &quot;Create Budget&quot; button highlighted." alt="Screenshot of the budgets and alerts page for a Google Cloud Billing Account. The &quot;Create Budget&quot; button highlighted." width="480" />

1. Enter a name for your budget, and then choose which projects you want to monitor. Then click "Next".

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_732.png" title="Screenshot of the form for setting budget scope for a Google Cloud Billing Account. Three things are highlighted:  1) the box for entering a &quot;Name&quot; for the budget, 2) the dropdown menu labeled &quot;Projects&quot; for selecting which Billing Projects are part of the budget, and 3) the &quot;Next&quot; button." alt="Screenshot of the form for setting budget scope for a Google Cloud Billing Account. Three things are highlighted:  1) the box for entering a &quot;Name&quot; for the budget, 2) the dropdown menu labeled &quot;Projects&quot; for selecting which Billing Projects are part of the budget, and 3) the &quot;Next&quot; button." width="480" />

1. For Budget Type, select "Specified amount". Enter the total budget amount for the month (you will set alerts at different thresholds in the next step). Click "**Next**" (do not click "Finish").

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_878.png" title="Screenshot of the form for setting budget amount for a Google Cloud Billing Account.  The drop-down menu labeled &quot;Budget type&quot; is highlighted and &quot;Specified amount&quot; is selected.  Also highlighted are the text box labeled &quot;Target amount&quot; and the &quot;Next&quot; button." alt="Screenshot of the form for setting budget amount for a Google Cloud Billing Account.  The drop-down menu labeled &quot;Budget type&quot; is highlighted and &quot;Specified amount&quot; is selected.  Also highlighted are the text box labeled &quot;Target amount&quot; and the &quot;Next&quot; button." width="480" />

1. Enter the threshold amounts where you want to receive an alert. We recommend starting with 50% and 90%. You can set other alerts if you prefer.

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_1025.png" title="Screenshot of the form for setting budget actions for a Google Cloud Billing Account.  The boxes for entering &quot;Percent of budget&quot; or &quot;Amount&quot; are highlighted.  The drop-down menu labeled &quot;Trigger on&quot; is highlighted and &quot;Actual&quot; is selected." alt="Screenshot of the form for setting budget actions for a Google Cloud Billing Account.  The boxes for entering &quot;Percent of budget&quot; or &quot;Amount&quot; are highlighted.  The drop-down menu labeled &quot;Trigger on&quot; is highlighted and &quot;Actual&quot; is selected." width="480" />

1. Check the box for "Email alerts to billing admins and users", then click "**Finish**". Now you (as the owner and admin), along with anyone you added with admin or user privileges (e.g. lab managers) will receive alerts when your lab members reach the specified spending thresholds. These emails will be sent to the Gmail accounts associated with the Billing Account.

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_1169.png" title="Screenshot of the form for setting budget alerts for a Google Cloud Billing Account.  The checkbox labeled &quot;Email alerts to billing admins and users&quot; is highligheted and checked.  The &quot;Finish&quot; button is highlighted." alt="Screenshot of the form for setting budget alerts for a Google Cloud Billing Account.  The checkbox labeled &quot;Email alerts to billing admins and users&quot; is highligheted and checked.  The &quot;Finish&quot; button is highlighted." width="480" />

1. You can edit your budgets at any time by going to Billing > Budgets & alerts, and clicking on the name of the budget you want to edit.

    <img src="05-billing_modules_files/figure-html//1GBYTx25VzBFh7kI_elgMC0fmOMm4YhcfW8wIJ1EkmKY_g115e284bdc2_0_1314.png" title="Screenshot of the Google Cloud Billing Account Budgets and alerts overview.  Four items are highlighted illustrating how to view and edit an existing budget: 1) The top-left &quot;hamburger&quot; button for extending the drop-down menu, 2) the drop-down menu item &quot;Billing&quot;, 3) the submenu item &quot;Budgets &amp; alerts, 4) the name of a budget." alt="Screenshot of the Google Cloud Billing Account Budgets and alerts overview.  Four items are highlighted illustrating how to view and edit an existing budget: 1) The top-left &quot;hamburger&quot; button for extending the drop-down menu, 2) the drop-down menu item &quot;Billing&quot;, 3) the submenu item &quot;Budgets &amp; alerts, 4) the name of a budget." width="480" />
::::

## View Spend for Google Billing

:::: {.borrowed_chunk}

You can always check your current spend through the Google Billing console, but remember

- There is a reporting delay (~1 day), so you cannot immediately see what an analysis cost
- Costs are reported at the level of Workspaces, so if there are multiple people using a Workspace, you will not be able to determine which of them was responsible for the charges.

The Google Billing console displays information by Billing Account.  To view spending:
  
1. Log in to the [Google Cloud Platform](https://console.cloud.google.com/) console using the Google ID associated with your Google Cloud projects.

1. Open the dropdown menu on the top left and click on [Billing](https://console.cloud.google.com/billing).

    <img src="05-billing_modules_files/figure-html//1Ofs1As7XZWmxnaBOZvNYzAiuuaYgn1ce700eHyCNg2Y_g115e284bdc2_0_144.png" title="Screenshot of the Google Cloud Console drop-down menu, with &quot;Billing&quot; highlighted." alt="Screenshot of the Google Cloud Console drop-down menu, with &quot;Billing&quot; highlighted." width="480" />

1. You may be automatically directed to view a specific Billing Account. If you see information about a single account (and it’s not the one you’re interested in), you can get back to the list of all your Billing Accounts by clicking "Manage Billing Accounts" from the drop-down menu.

    <img src="05-billing_modules_files/figure-html//1Ofs1As7XZWmxnaBOZvNYzAiuuaYgn1ce700eHyCNg2Y_g115e284bdc2_0_295.png" title="Screenshot of an individual Google Cloud Billing Account with the drop-down menu item &quot;Manage Billing Accounts&quot; highlighted." alt="Screenshot of an individual Google Cloud Billing Account with the drop-down menu item &quot;Manage Billing Accounts&quot; highlighted." width="480" />

1. Click on the name of the Billing Account for the project you want to view.

    <img src="05-billing_modules_files/figure-html//1Ofs1As7XZWmxnaBOZvNYzAiuuaYgn1ce700eHyCNg2Y_g115e284bdc2_0_150.png" title="Screenshot of Google Cloud Billing Accounts Overview. A Billing Account name is highlighted." alt="Screenshot of Google Cloud Billing Accounts Overview. A Billing Account name is highlighted." width="480" />

1. Look at the top of the **Overview** tab to see your month-to-date spending.
    
    <img src="05-billing_modules_files/figure-html//1Ofs1As7XZWmxnaBOZvNYzAiuuaYgn1ce700eHyCNg2Y_g1149729109c_0_0.png" title="Screenshot of a Google Cloud Billing Account Overview." alt="Screenshot of a Google Cloud Billing Account Overview." width="480" />

1. Scroll further down the **Overview** tab to show your top projects.
    
    <img src="05-billing_modules_files/figure-html//1Ofs1As7XZWmxnaBOZvNYzAiuuaYgn1ce700eHyCNg2Y_g1149729109c_0_143.png" title="Screenshot of a Google Cloud Billing Account top projects." alt="Screenshot of a Google Cloud Billing Account top projects." width="480" />

1. Click on the **Reports** tab to see more detailed information about each of your projects.  This is probably the most useful tab for exploring costs of individual projects over time.

    <img src="05-billing_modules_files/figure-html//1Ofs1As7XZWmxnaBOZvNYzAiuuaYgn1ce700eHyCNg2Y_g1149729109c_0_433.png" title="Screenshot of a Google Cloud Billing Account Reports tab." alt="Screenshot of a Google Cloud Billing Account Reports tab." width="480" />

1. Click on the **Cost table** tab to obtain a convenient table of spending per project.
    
    <img src="05-billing_modules_files/figure-html//1Ofs1As7XZWmxnaBOZvNYzAiuuaYgn1ce700eHyCNg2Y_g1149729109c_0_577.png" title="Screenshot of a Google Cloud Billing Account Cost table tab." alt="Screenshot of a Google Cloud Billing Account Cost table tab." width="480" />
::::

## Create Terra Billing Project

:::: {.borrowed_chunk}

1. [Launch Terra](https://anvil.terra.bio/#workspaces) and sign in with your Google account.  If this is your first time logging in to Terra, you will need to accept the Terms of Service.

1. In the drop-down menu on the left, navigate to "Billing". Click the triple bar in the top left corner to access the menu. Click the arrow next to your name to expand the menu, then click "Billing".  You can also navigate there directly with this link: https://anvil.terra.bio/#billing

    <img src="05-billing_modules_files/figure-html//1POwxqv4p6AfPHJlN9VNq0TaT44fA2RAFSpIERIMHdWU_g116f8d759be_0_2.png" title="Screenshot of the Terra drop-down menu.  Three items are highlighted: 1) the &quot;hamburger&quot; button for extending the drop-down menu, 2) the arrow next to your username, for extending the drop-down submenu, and 3) the submenu item &quot;Billing&quot;." alt="Screenshot of the Terra drop-down menu.  Three items are highlighted: 1) the &quot;hamburger&quot; button for extending the drop-down menu, 2) the arrow next to your username, for extending the drop-down submenu, and 3) the submenu item &quot;Billing&quot;." width="100%" />

1. On the Billing page, click the "+ CREATE" button to create a new Billing Project. If prompted, select the Google account to use.  If prompted, give Terra permission to manage Google Cloud Platform billing accounts.

    <img src="05-billing_modules_files/figure-html//1POwxqv4p6AfPHJlN9VNq0TaT44fA2RAFSpIERIMHdWU_g116f8d759be_0_149.png" title="Screenshot of the Terra Billing Page.  The &quot;plus&quot; button next to &quot;Billing Projects&quot; is highlighted." alt="Screenshot of the Terra Billing Page.  The &quot;plus&quot; button next to &quot;Billing Projects&quot; is highlighted." width="100%" />

1. Enter a **unique** name for your Terra Billing Project and select the appropriate Google Billing Account. The name of the Terra Billing Project must:
    + Only contain lowercase letters, numbers and hyphens
    + Start with a lowercase letter
    + Not end with a hyphen
    + Be between 6 and 30 characters

    <img src="05-billing_modules_files/figure-html//1POwxqv4p6AfPHJlN9VNq0TaT44fA2RAFSpIERIMHdWU_g116f8d759be_0_293.png" title="Screenshot of the Terra Add Billing Project dialog box." alt="Screenshot of the Terra Add Billing Project dialog box." width="100%" />

The page doesn't always update as soon as the Billing Project is created.  If it's been a couple of minutes and you don't see a change, try refreshing the page.
::::

## Add Member to Terra Billing Project

:::: {.borrowed_chunk}

1. [Launch Terra](https://anvil.terra.bio/#workspaces) and sign in with your Google account.

1. In the drop-down menu on the left, navigate to "Billing". Click the triple bar in the top left corner to access the menu. Click the arrow next to your name to expand the menu, then click "Billing".  You can also navigate there directly with this link: https://anvil.terra.bio/#billing

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g116f8d759be_0_2.png" title="Screenshot of the Terra drop-down menu.  Three items are highlighted: 1) the &quot;hamburger&quot; button for extending the drop-down menu, 2) the arrow next to your username, for extending the drop-down submenu, and 3) the submenu item &quot;Billing&quot;." alt="Screenshot of the Terra drop-down menu.  Three items are highlighted: 1) the &quot;hamburger&quot; button for extending the drop-down menu, 2) the arrow next to your username, for extending the drop-down submenu, and 3) the submenu item &quot;Billing&quot;." width="100%" />

1. Click “Owned by You” and find the Billing Project.  If you do not see the Billing Project in this list, then you are not an Owner and do not have permission to add members.

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g116f8d759be_0_149.png" title="Screenshot of the Terra Billing Projects menu.  The submenu &quot;Owned by you&quot; is highlighted and has been expanded, showing a list of Billing Projects below." alt="Screenshot of the Terra Billing Projects menu.  The submenu &quot;Owned by you&quot; is highlighted and has been expanded, showing a list of Billing Projects below." width="100%" />

1. Click on the name of the Billing Project.

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g1edc2edcaf8_1_29.png" title="Screenshot of the Terra Billing Projects menu.  The submenu has been expanded, and the name of one of the Billing Projects is highlighted." alt="Screenshot of the Terra Billing Projects menu.  The submenu has been expanded, and the name of one of the Billing Projects is highlighted." width="100%" />

1. Click on the “Members” tab to view and manage members.  Then click the “Add User” button.

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g1edc2edcaf8_1_14.png" title="Screenshot of a Terra Billing Project management page.  The tab labeled &quot;Members&quot; is highlighed and has been selected, and the button labeled &quot;Add User&quot; is highlighted." alt="Screenshot of a Terra Billing Project management page.  The tab labeled &quot;Members&quot; is highlighed and has been selected, and the button labeled &quot;Add User&quot; is highlighted." width="100%" />
    
1. Enter the email address of the user or group you’d like to add the the Billing Project.
    - If adding an individual, make sure to enter the account that they use to access AnVIL.
    - If adding a Terra Group, use the Group email address, which can be found on the Terra Group management page.

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g1edc2edcaf8_1_36.png" title="Screenshot of the dialog box for adding users to a Terra Billing Project.  The textbox labeled &quot;User email&quot; is highlighed and has been filled in." alt="Screenshot of the dialog box for adding users to a Terra Billing Project.  The textbox labeled &quot;User email&quot; is highlighed and has been filled in." width="100%" />

1. If this user or group will need to add and remove other users of the Billing Project, check the Owner box.  Otherwise leave it unchecked.
    - It’s often a good idea to have at least one other Owner of a Billing Project in order to avoid getting locked out, in case the original owner leaves or loses access to their account.

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g1edc2edcaf8_1_42.png" title="Screenshot of the dialog box for adding users to a Terra Billing Project.  The checkbox labeled &quot;Can manage users (Owner)&quot; is highlighed." alt="Screenshot of the dialog box for adding users to a Terra Billing Project.  The checkbox labeled &quot;Can manage users (Owner)&quot; is highlighed." width="100%" />

1. Click “ADD USER”.

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g1edc2edcaf8_1_48.png" title="Screenshot of the dialog box for adding users to a Terra Billing Project.  The button labeled &quot;ADD USER&quot; is highlighed." alt="Screenshot of the dialog box for adding users to a Terra Billing Project.  The button labeled &quot;ADD USER&quot; is highlighed." width="100%" />

1.  You should now see the user or group listed in the Billing Project members, along with the appropriate role.  They should now be able to use the Billing Project to fund work on AnVIL.

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g1edc2edcaf8_1_55.png" title="Screenshot of a Terra Billing Project member management page.  A user email and role are highlighted." alt="Screenshot of a Terra Billing Project member management page.  A user email and role are highlighted." width="100%" />
    
1. If you need to remove members or modify their roles, you can do so at any time by clicking the teardrop button next to their name.

    <img src="05-billing_modules_files/figure-html//10-YvYQqI2y32ErihJJMbyLAL4nPIkPA2MLk_6Ee7fXw_g1edc2edcaf8_1_62.png" title="Screenshot of a Terra Billing Project member management page.  The teardrop button for one user is highlighted." alt="Screenshot of a Terra Billing Project member management page.  The teardrop button for one user is highlighted." width="100%" />
::::

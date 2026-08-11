import csv
import json
import os
import urllib.request
import urllib.error
from datetime import datetime
from zoneinfo import ZoneInfo


# ============================================================
# SETTINGS
# ============================================================

RESEND_API_KEY = os.environ["RESEND_API_KEY"]

RECIPIENTS = [
    email.strip()
    for email in os.environ["EMAIL_RECIPIENTS"].split(",")
    if email.strip()
]

DASHBOARD_URL = os.environ["DASHBOARD_URL"]

# For testing with Resend.
# This can send only to the email address associated with
# your Resend account until you verify your own domain.
FROM_EMAIL = "WA Economic Dashboard <onboarding@resend.dev>"


# ============================================================
# READ DASHBOARD DATA
# ============================================================

with open(
    "data/dashboard_summary.csv",
    newline="",
    encoding="utf-8-sig"
) as f:
    rows = list(csv.DictReader(f))


figures = {}

for row in rows:
    figures[row["indicator"]] = row["latest_display"]


def get(name):
    return figures.get(name, "n/a")


# ============================================================
# DATE
# ============================================================

perth_time = datetime.now(ZoneInfo("Australia/Perth"))
today = perth_time.strftime("%d %B %Y")


# ============================================================
# BUILD EMAIL
# ============================================================

html = f"""
<html>

<body style="
    font-family: Arial, Helvetica, sans-serif;
    color: #222222;
    background-color: #ffffff;
">

<div style="
    max-width: 650px;
    margin: 0 auto;
">

    <h2 style="margin-bottom: 4px;">
        WA Economic Indicators
    </h2>

    <p style="
        color: #666666;
        margin-top: 0;
        margin-bottom: 24px;
    ">
        {today}
    </p>


    <table
        cellpadding="10"
        cellspacing="0"
        width="100%"
        style="
            border-collapse: collapse;
            border: 1px solid #dddddd;
        "
    >

        <tr style="background-color: #f5f5f5;">
            <th
                align="left"
                style="border-bottom: 1px solid #dddddd;"
            >
                Indicator
            </th>

            <th
                align="right"
                style="border-bottom: 1px solid #dddddd;"
            >
                Latest
            </th>
        </tr>


        <tr>
            <td style="border-bottom: 1px solid #eeeeee;">
                WA unemployment rate
            </td>

            <td
                align="right"
                style="border-bottom: 1px solid #eeeeee;"
            >
                <b>{get("WA unemployment rate")}</b>
            </td>
        </tr>


        <tr>
            <td style="border-bottom: 1px solid #eeeeee;">
                Perth headline CPI
            </td>

            <td
                align="right"
                style="border-bottom: 1px solid #eeeeee;"
            >
                <b>{get("Perth headline CPI")}</b>
            </td>
        </tr>


        <tr>
            <td style="border-bottom: 1px solid #eeeeee;">
                Australia underlying CPI
            </td>

            <td
                align="right"
                style="border-bottom: 1px solid #eeeeee;"
            >
                <b>{get("Australia underlying CPI")}</b>
            </td>
        </tr>


        <tr>
            <td style="border-bottom: 1px solid #eeeeee;">
                Australia CPI short-term trend
            </td>

            <td
                align="right"
                style="border-bottom: 1px solid #eeeeee;"
            >
                <b>{get("Australia CPI short-term trend")}</b>
            </td>
        </tr>


        <tr>
            <td style="border-bottom: 1px solid #eeeeee;">
                WA dwelling approvals
            </td>

            <td
                align="right"
                style="border-bottom: 1px solid #eeeeee;"
            >
                <b>{get("WA dwelling approvals")}</b>
            </td>
        </tr>


        <tr>
            <td style="border-bottom: 1px solid #eeeeee;">
                Lowest listed ULP price — Perth
            </td>

            <td
                align="right"
                style="border-bottom: 1px solid #eeeeee;"
            >
                <b>{get("Lowest listed ULP price — Perth")}</b>
            </td>
        </tr>


        <tr>
            <td>
                Lowest listed diesel price — Perth
            </td>

            <td align="right">
                <b>{get("Lowest listed diesel price — Perth")}</b>
            </td>
        </tr>

    </table>


    <p style="margin-top: 28px;">

        <a
            href="{DASHBOARD_URL}"
            style="
                display: inline-block;
                padding: 11px 18px;
                background-color: #1f5a89;
                color: white;
                text-decoration: none;
                border-radius: 4px;
            "
        >
            View full WA Economic Dashboard
        </a>

    </p>


    <p style="
        margin-top: 28px;
        font-size: 12px;
        color: #777777;
    ">
        Data sources include the Australian Bureau of Statistics,
        FuelWatch and WA Fuel Finder.
    </p>

</div>

</body>
</html>
"""


# ============================================================
# SEND THROUGH RESEND
# ============================================================

payload = {
    "from": FROM_EMAIL,
    "to": RECIPIENTS,
    "subject": f"WA Economic Indicators — {today}",
    "html": html
}


request = urllib.request.Request(
    "https://api.resend.com/emails",
    data=json.dumps(payload).encode("utf-8"),
    headers={
        "Authorization": f"Bearer {RESEND_API_KEY}",
        "Content-Type": "application/json"
    },
    method="POST"
)


try:

    with urllib.request.urlopen(request) as response:

        result = json.loads(
            response.read().decode("utf-8")
        )

        print("Email successfully sent.")
        print(f"Resend email ID: {result.get('id')}")
        print(f"Recipients: {len(RECIPIENTS)}")


except urllib.error.HTTPError as error:

    error_message = error.read().decode("utf-8")

    print("Resend failed to send the email.")
    print(f"HTTP status: {error.code}")
    print(f"Resend response: {error_message}")

    raise


except Exception as error:

    print("Unexpected error while sending email:")
    print(str(error))

    raise

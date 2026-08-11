import csv
import os
import smtplib
from email.message import EmailMessage
from datetime import datetime

USERNAME = os.environ["EMAIL_USERNAME"]
PASSWORD = os.environ["EMAIL_APP_PASSWORD"]
RECIPIENTS = [
    x.strip()
    for x in os.environ["EMAIL_RECIPIENTS"].split(",")
    if x.strip()
]

DASHBOARD_URL = os.environ["DASHBOARD_URL"]

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

today = datetime.now().strftime("%d %B %Y")

html = f"""
<html>
<body style="font-family:Arial,sans-serif; color:#222;">
  <h2>WA Economic Indicators</h2>
  <p>{today}</p>

  <table cellpadding="8"
         cellspacing="0"
         style="border-collapse:collapse;">
    <tr>
      <td><b>WA unemployment rate</b></td>
      <td>{get("WA unemployment rate")}</td>
    </tr>

    <tr>
      <td><b>Perth headline CPI</b></td>
      <td>{get("Perth headline CPI")}</td>
    </tr>

    <tr>
      <td><b>Australia underlying CPI</b></td>
      <td>{get("Australia underlying CPI")}</td>
    </tr>

    <tr>
      <td><b>Australia CPI short-term trend</b></td>
      <td>{get("Australia CPI short-term trend")}</td>
    </tr>

    <tr>
      <td><b>WA dwelling approvals</b></td>
      <td>{get("WA dwelling approvals")}</td>
    </tr>

    <tr>
      <td><b>Lowest listed ULP price — Perth</b></td>
      <td>{get("Lowest listed ULP price — Perth")}</td>
    </tr>

    <tr>
      <td><b>Lowest listed diesel price — Perth</b></td>
      <td>{get("Lowest listed diesel price — Perth")}</td>
    </tr>
  </table>

  <p style="margin-top:20px;">
    <a href="{DASHBOARD_URL}">
      View the full WA Economic Indicators dashboard
    </a>
  </p>

  <p style="font-size:12px;color:#666;">
    ABS, FuelWatch and WA Fuel Finder data.
  </p>
</body>
</html>
"""

msg = EmailMessage()

msg["Subject"] = f"WA Economic Indicators — {today}"
msg["From"] = USERNAME

# Hide the mailing list from recipients.
msg["To"] = "undisclosed-recipients:;"

msg.set_content(
    f"WA Economic Indicators — {today}\n\n"
    f"View dashboard: {DASHBOARD_URL}"
)

msg.add_alternative(html, subtype="html")

with smtplib.SMTP_SSL("smtp.gmail.com", 465) as server:
    server.login(USERNAME, PASSWORD)
    server.send_message(
        msg,
        from_addr=USERNAME,
        to_addrs=RECIPIENTS
    )

print(f"Email sent to {len(RECIPIENTS)} recipient(s).")

{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 2,
   "id": "011a2c07-87df-4420-ad0a-8f2be7bd8eef",
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Type of mydf: <class 'pandas.core.frame.DataFrame'>\n",
      "Type of mydf['date']: object\n",
      "         date                                        description lang  \\\n",
      "0  2012/12/31  The Kars–Tbilisi–Baku railway across the Cauca...   en   \n",
      "\n",
      "      category1 granularity  \n",
      "0  Date unknown        year  \n"
     ]
    }
   ],
   "source": [
    "import subprocess\n",
    "\n",
    "# Define the command to download the file\n",
    "command = 'wget -O dates.json \"https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en\"'\n",
    "\n",
    "# Execute the command\n",
    "subprocess.call(command, shell=True)\n",
    "\n",
    "# Print the content of dates.json\n",
    "subprocess.call('cat dates.json', shell=True)\n",
    "\n",
    "import pandas as pd\n",
    "\n",
    "# Load the JSON data into a DataFrame\n",
    "df = pd.read_json('dates.json')\n",
    "\n",
    "# Assuming the JSON structure is nested and we focus on 'result' part\n",
    "mydf = pd.DataFrame(df['result'].tolist()[1:])  # Adjusted for Python's indexing\n",
    "\n",
    "# Check the types\n",
    "print(f\"Type of mydf: {type(mydf)}\")\n",
    "print(f\"Type of mydf['date']: {mydf['date'].dtype}\")\n",
    "\n",
    "# List the first n rows\n",
    "n = 5\n",
    "print(mydf.head(n))\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "0dc96e4a-b13d-446d-ba73-0866ac8776d7",
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3 (ipykernel)",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.9.12"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}

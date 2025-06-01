// script.js

// This script is designed to work with a Shiny R application.
// It provides client-side functionalities such as:
// 1. Notifying the Shiny server that the JavaScript file has loaded.
// 2. Scrolling to a specific HTML element when a custom message is received from Shiny.
// 3. Copying the content of the prediction results table to the clipboard.

// Ensure the document is fully loaded before executing the script.
document.addEventListener('DOMContentLoaded', function() {
    console.log("script.js loaded and DOM content parsed."); // Log to confirm script execution start

    // 1. Notify Shiny that JavaScript is loaded.
    // This sends a custom message to the Shiny server, which can be
    // observed by `Shiny.addCustomMessageHandler` in the R server logic.
    // This helps confirm that `script.js` is being loaded and executed by the browser.
    if (typeof Shiny !== 'undefined') {
        Shiny.onInputChange("js_loaded_message", true);
        console.log("Sent 'js_loaded_message' to Shiny."); // Log to confirm message sent
    } else {
        console.warn("Shiny object not found. Is Shiny.js loaded?"); // Warning if Shiny not available
    }

    // 2. Handle custom message from Shiny to scroll to a specific element.
    // The R server sends a message of type "scroll_to_results" with an 'id'.
    // This listener will find the element by its ID and scroll it into view.
    if (typeof Shiny !== 'undefined') {
        Shiny.addCustomMessageHandler("scroll_to_results", function(message) {
            console.log("Received 'scroll_to_results' message from Shiny:", message);
            const element = document.getElementById(message.id);
            if (element) {
                element.scrollIntoView({ behavior: 'smooth', block: 'start' });
                console.log(`Scrolled to element with ID: ${message.id}`);
            } else {
                console.warn(`Element with ID '${message.id}' not found for scrolling.`);
            }
        });
    }

    // 3. Handle copying the results table to the clipboard.
    // This attaches a click event listener to the "Copy Table to Clipboard" button.
    // It extracts data from the DataTable and copies it as tab-separated values.
    const copyButton = document.getElementById('copy_table'); // Get the copy button by its ID
    if (copyButton) {
        copyButton.addEventListener('click', function() {
            console.log("Copy Table button clicked.");
            const table = document.getElementById('results_table'); // Get the table element by its ID
            if (table) {
                let tableText = '';
                // Get table headers
                const headers = table.querySelectorAll('thead th');
                headers.forEach((header, index) => {
                    tableText += header.innerText.trim();
                    if (index < headers.length - 1) {
                        tableText += '\t'; // Use tab as a separator for columns
                    }
                });
                tableText += '\n'; // New line after headers

                // Get table rows (body)
                const rows = table.querySelectorAll('tbody tr');
                rows.forEach(row => {
                    const cells = row.querySelectorAll('td');
                    cells.forEach((cell, index) => {
                        tableText += cell.innerText.trim();
                        if (index < cells.length - 1) {
                            tableText += '\t'; // Use tab as a separator for columns
                        }
                    });
                    tableText += '\n'; // New line after each row
                });

                // Attempt to copy the text to the clipboard.
                const tempTextArea = document.createElement('textarea');
                tempTextArea.value = tableText;
                document.body.appendChild(tempTextArea);
                tempTextArea.select();
                try {
                    const successful = document.execCommand('copy');
                    const msg = successful ? 'successful' : 'unsuccessful';
                    console.log('Copying text command was ' + msg);
                    showTemporaryMessage("Table copied to clipboard!", "success");
                } catch (err) {
                    console.error('Oops, unable to copy', err);
                    showTemporaryMessage("Failed to copy table.", "error");
                }
                document.body.removeChild(tempTextArea); // Clean up the temporary textarea
            } else {
                console.warn("Results table with ID 'results_table' not found for copying.");
                showTemporaryMessage("Results table not found.", "error");
            }
        });
    } else {
        console.warn("Copy Table button with ID 'copy_table' not found.");
    }

    // Helper function to show a temporary message to the user.
    function showTemporaryMessage(message, type) {
        let messageDiv = document.getElementById('app-message-box');
        if (!messageDiv) {
            messageDiv = document.createElement('div');
            messageDiv.id = 'app-message-box';
            Object.assign(messageDiv.style, {
                position: 'fixed',
                bottom: '20px',
                left: '50%',
                transform: 'translateX(-50%)',
                padding: '10px 20px',
                borderRadius: '5px',
                color: 'white',
                zIndex: '1000',
                opacity: '0',
                transition: 'opacity 0.5s ease-in-out',
                textAlign: 'center'
            });
            document.body.appendChild(messageDiv);
        }

        if (type === "success") {
            messageDiv.style.backgroundColor = 'rgba(40, 167, 69, 0.9)'; // Green
        } else if (type === "error") {
            messageDiv.style.backgroundColor = 'rgba(220, 53, 69, 0.9)'; // Red
        } else {
            messageDiv.style.backgroundColor = 'rgba(0, 0, 0, 0.7)'; // Default dark
        }

        messageDiv.innerText = message;
        messageDiv.style.opacity = '1';

        setTimeout(() => {
            messageDiv.style.opacity = '0';
        }, 3000);
    }
});

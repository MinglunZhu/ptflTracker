// Function to check orientation and set showLegend value
function updateLegendByOrientation() {
    // isLandscape will be true if width > height
    Shiny.setInputValue('showLegend_orientation', window.innerWidth > window.innerHeight);
}

$(document).ready(function() {
    const   HDR = $('.main-header'), // Target the header
            TGL_BTN = $('#header-toggle-btn'); // Target the button
    let isMouseOverHdr = false;

    TGL_BTN.on('click', function() {
        HDR.addClass('active');
        // Optional: maybe hide the button when header is active?
        // toggleBtn.hide();
    });

    // Keep the header open when interacting with it
    HDR.on('mouseenter', function() {
        isMouseOverHdr = true;
    });

    function checkDeactivateHeader() {
        // Deactivate only if mouse is NOT over header AND no dropdowns are shown
        const ANY_OPEN = $('.dropdown-menu.show').length > 0 || $('.selectize-dropdown.multi').is(':visible'); // Check Bootstrap and Selectize dropdowns

        // Deactivate only if:
        // 1. Header currently IS active
        // 2. Mouse is NOT over the header area
        // 3. NO dropdown menu is open
        if (HDR.hasClass('active') && !isMouseOverHdr && !ANY_OPEN) {
            HDR.removeClass('active');
            // Optional: show the button again if it was hidden
            // toggleBtn.show();
        }
    }

    HDR.on('mouseleave', function() {
        isMouseOverHdr = false;

        // Check conditions immediately on mouse leave
        checkDeactivateHeader();
    });

    // Use Bootstrap's event delegation for potentially dynamic dropdowns
    $(document).on('hidden.bs.dropdown', '.dropdown', function () {
        // Check conditions when a dropdown closes
        checkDeactivateHeader();
    });

    // Initialize after Shiny is ready
    $(document).on('shiny:connected', function() {
        console.log('Shiny is connected, checking orientation');
        // Update value when orientation changes
        window.addEventListener('orientationchange', updateLegendByOrientation);
        // Also handle resize for browsers/devices that don't support orientationchange
        window.addEventListener('resize', updateLegendByOrientation);

        updateLegendByOrientation();
    });
});